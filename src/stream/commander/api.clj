(ns stream.commander.api
  (:require [stream.commander.systemd :as sys]
            [stream.commander.journal :as journal]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [slingshot.slingshot :refer [throw+]]
            [outpace.config :refer [defconfig]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]
            [slingshot.slingshot :refer [throw+ try+]])
  (:import [java.io File]
           [java.nio.file Files LinkOption CopyOption StandardCopyOption Paths]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    Globals, Structures and State    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: specs
;; TODO: sanitize custom flags

(defrecord process
    [id process-name unit-name monitor supervisor ffmpeg-config problems])

(defn process=
  "Tests `proc-1` and `proc-2` for equality."
  [proc-1 proc-2]
  (loop [[key & keys] [:id :process-name :config]]
    (if key
      (if (= (get proc-1 key) (get proc-2 key))
        (recur keys)
        false)
      true)))

(def ^:private processes (ref {}))
(def ^:private dbus-monitor (chan))
(def ^:private master-monitor (chan))

(def monitor (a/pub master-monitor :type))

(defconfig default-ffmpeg-config
  {:ffmpeg-path "/usr/bin/ffmpeg" :rtsp-transport "tcp" :custom-flags ""
   :audio-bitrate "128k" :audio-channels 1 :audio-sampling-rate 44100
   :rtsp-user nil :rtsp-password nil :buffer-size "100M"})

(defconfig ^{:validate [number? "Must be a number."]} default-timeout 1000)
(defconfig ^{:validate [#(let [path (Paths/get % (into-array String []))]
                           (Files/exists path
                                         (into-array LinkOption []))
                           (Files/isWritable path)
                           (Files/isReadable path))
                        "Path must exist, be writable, be readable."]}
  processdb-directory "./processes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Utilities              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- input-string
  [user pass ip port profile]
  (str "rtsp://"
       (if (and user pass) (str user ":" pass "@"))
       ip ":" port "/" profile))

(defn- generate-process-id
  "Generates a guaranteed unique ID for the process."
  []
  (loop []
    (let [id (str (java.util.UUID/randomUUID))]
      (if (contains? @processes id)
        (recur)
        id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Error Diagnose           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn diagnose-error
  "Deduces the problem from the error `message` and the process
  information `proc` and returns it.

  The returned codes are:
    `:source` The streaming source can't be reached.
    `:remote` The streaming remote can't be reached.
    `:os` Launching the process failed at the os level.
          Executable not found or something else.
          Most likely not recoverable.
    `:user` The user killed the process. This ain't an error.
    `:unknown` The catchall."
  [message proc]
  (cond
    (string/includes? message (:cam-ip (:ffmpeg-config proc)))
    :source

    (or (re-find #"Operation not permitted" message)
       (re-find #"Input/output" message)
       (string/includes? message (:stream-server (:ffmpeg-config proc))))
    :remote

    (or (re-find #"spawn" message)
       (re-find #"niceness" message))
    :os

    (or (re-find #"SIGINT" message)
       (re-find #"SIGKILL" message))
    :user

    :else :unknown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Monitoring             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-dbus-event
  "Parses a dbus state `update` into an event."
  [update id]
  (condp = (:ActiveState update)
    "failed" {:event :failed
              :reason (keyword (:SubState update))
              ;; we take the last message because the journal does not
              ;; distinct between message types from the executable
              :error
              (let [process (get @processes id)]
                (:message
                 (first
                  (journal/get-logs!
                   (:unit-name process)
                   :number 1
                   :executable (:ffmpeg-path (:ffmpeg-config process))))))}
    "active" {:event :active :reason (keyword (:SubState update))}
    "activating" {:event :activating :reason (keyword (:SubState update))}
    "deactivating" {:event :deactivating :reason (keyword (:SubState update))}
    "inactive" {:event :inactive :reason (keyword (:SubState update))}
    nil))

(defn- put-process-event!
  [id data]
  (a/put! master-monitor {:type :process-event
                          :id id
                          :data data}))

(defn- handle-monitor-event!
  "Handles a service monitor event in the supervisor thread."
  [id event queue]
  (let [{type :type
         data :data} event]
    (condp = type
      :dbus
      (if-let [parsed (parse-dbus-event data id)]
        (do (trace "Dbus Event" id parsed)
            (put-process-event! id parsed)
            (reduce (fn [queue element]
                      (if (or (if-let [watch-for (:event element)]
                               (= (:event parsed) watch-for)
                               false)
                             (if-let [matcher (:matcher element)]
                               (matcher parsed)
                               false))
                        (do
                          (trace id
                                 "Delivering event:" parsed)
                          (deliver (:promise element) parsed)
                          queue)
                        (conj queue element))) [] queue))
        queue)
      queue)))

(defn- handle-control-event!
  "Handles a control message in the supervisor thread.

  Unknown events are a noop."
  [event queue]
  (condp = (:command event)
    :wait-for
    (do (trace "Waiting for" event)
        (conj queue
              {:timeout (timeout (:timeout event))
               :event (:event event)
               :matcher (:matcher event)
               :promise (:promise event)}))
    queue))

(defn- handle-timeout-event!
  "Handles a timed-out waiter in the supervisor thread.
  Removes that waiter from the queue and resolves its promise with a
  timeout message."
  [channel queue]
  (trace "Handling timeout.")
  (first
   (reduce
    (fn [[queue found] element]
      (if (and (not found) (= (:timeout element) channel))
        (do (deliver (:promise element)
                     :timeout)
            [queue true])
        [(conj queue element) found]))
    [[] false] queue)))

;; TODO: specs
;; TODO: document event types
(defn- attach-supervisor!
  "Attaches a monitor consumer to the monitor channel of a process.
  It processes all status messages from the process and forwards them
  to the main monitor if necessary."
  [id monitor]
  (let [control (chan)]
    (thread
      (trace "Monitoring" id)
      (loop [queue []]
        (let [[event channel]
              (alts!! (conj (map :timeout queue) monitor control))]
          (condp = channel
            monitor
            (recur (handle-monitor-event! id event queue))

            control
            (if event
              (recur (handle-control-event! event queue))
              (do                       ; channel closed -> stopp
                (trace "Stopping monitoring of " id)
                (put-process-event! id {:event :monitor-deleted})
                (doseq [waiter queue]
                  (deliver (:promise waiter) :timeout))))

            (recur (handle-timeout-event! channel queue))))))
    control))

(defn wait-for!
  "Installs a waiter in the supervisor thread for a process.
  Takes the `supervisor` and some keyword arguments described
  below. Returns a promise.

  The promise resolves to `:timeout` in case of a timeout and to the
  event data in case the event occurs.

  :event
  :  The event to wait for. For example `:active`

  :matcher
  :  A predicate that takes a parsed event returned
     by [[parse-dbus-event]]. If that predicate returns true,
     the promise will be fulfilled.

  :timeout
  :  Timeout in msec to wait. If the timeout is triggered, the
     promise will be fulfilled with the value `:timeout`. Defaults to one
     second.

  Either `:event` or `:matcher` or both have to be given. `:event`
  takes precedence.
  "
  [process & {:keys [event timeout matcher]
                 :or {timeout default-timeout}}]
  (let [prom (promise)
        supervisor (:supervisor process)]
    (if (and (not event) (not matcher))
      (throw+ {:type ::commander-error
               :detail-type ::create-watch-error
               :message "Either event or matcher have to be specified!"})
      (a/put! supervisor
              {:command :wait-for :matcher matcher
               :event event :promise prom :timeout timeout}))
    prom))

(defn multi-event-matcher
  "Matches any of the given events."
  [& events]
  #(some #{(:event %1)} [:inactive :failed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;          Process Management         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ffmpeg-command
  "Generate the ffmpeg command from the config."
  [config]
  (let [{:keys
         [cam-ip
          cam-rtsp-port
          rtsp-user
          rtsp-password
          profile
          stream-server
          stream-key
          ffmpeg-path
          custom-flags
          audio-bitrate
          audio-channels
          audio-sampling-rate
          rtsp-transport
          buffer-size]} config]
    (str ffmpeg-path
         " -rtsp_transport " rtsp-transport
         " -i " (input-string rtsp-user rtsp-password cam-ip cam-rtsp-port profile)
         " -vcodec copy -b:a " audio-bitrate
         " -ac " audio-channels
         " -ar " audio-sampling-rate
         " -f flv -bufsize " buffer-size
         " -tune film " stream-server "/" stream-key
         " -loglevel error")))

(defn- sanitize-process-name [name]
  (string/lower-case (string/replace name #"\W" "-")))

(defn get-process!
  "Get the process with the id."
  [id]
  (get @processes id))

(declare delete-processdb-file!)
(defn delete-process!
  "Deletes a process from the process map, stops it and deletes the unit file.
  Returns `true` on success, `false` otherwise."
  [proc]
  (info "Removing process with ID:" (:process-name proc))
  (let [{:keys [unit-name monitor]} proc
        [monitor close] monitor]
    (close! (:supervisor proc))
    (sys/remove-service! unit-name)
    (close)
    (dosync (commute processes dissoc (:id proc)))
    (delete-processdb-file! (:id proc))
    true))

(declare save-process!)
(defn create-process!
  "Creates a process with the name `process-name` and the
  `ffmpeg-config`, adds it to the registry and assigns a monitoring
  channel to it. An `id` is generated if not specified. When the
  specified `id` is already present in the registry it is replaced.

  Returns the process."
  ([process-name ffmpeg-config id]
   (if-let [proc (get-process! id)]
     (do (info "Replacing process with ID:" id)
         (delete-process! proc))
     (info "Creating process " process-name))
   (let [ffmpeg-config (merge default-ffmpeg-config ffmpeg-config)
         unit-name (str (sanitize-process-name process-name)
                        "-" id)
         path (sys/create-service! unit-name
                                   (ffmpeg-command ffmpeg-config)
                                   (get ffmpeg-config :description
                                        "FFMPEG streaming process, created by `stream`."))
         monitor (sys/create-monitor! unit-name)
         supervisor (attach-supervisor! id (first monitor))
         process (->process id process-name unit-name monitor supervisor ffmpeg-config #{})]
     (dosync
      (commute processes assoc id process))
     (save-process! process)
     process))
  ([process-name ffmpeg-config]
   (create-process! process-name ffmpeg-config (generate-process-id))))

(defn delete-all-processes! []
  "Deletes all processes."
  (doseq [[_ proc] @processes]
    (delete-process! proc)))

(defn get-process-state!
  "Queries wether a process is running."
  [proc]
  (sys/get-service-state! (:unit-name proc)))

;; These control functions do not wait until a "final state" is
;; reached but resolve once a new, maybe unstable, state has been reached.

(defn start-process!
  "Starts the service associated to the process `proc`. Returns a
  promise that resolves to event `:failed` or `:active` or times out
  after `timeout` ms."
  ([proc timeout]
   (let [prom
         (wait-for! proc :matcher (multi-event-matcher [:active :failed]))]
     (sys/start-service! (:unit-name proc))
     prom))
  ([proc]
   (start-process! proc default-timeout)))

(defn stop-process!
  "Stops the service associated to the process `proc`. Returns a promise
  that resolves to event `:failed` or `:inactive` or times out after
  `timeout` ms."
  ([proc timeout]
   (let [prom
         (wait-for! proc :matcher (multi-event-matcher [:inactive :failed]))]
     (sys/stop-service! (:unit-name proc))
     prom))
  ([proc]
   (start-process! proc default-timeout)))

(defn restart-process!
  "Restarts a process `proc` and wait for stop and start to happen
  within `timeout`."
  ([proc timeout]
   @(stop-process! proc timeout)
   (start-process! proc timeout))
  ([proc]
   (restart-process! proc default-timeout)))

(defn process-running?
  "Queries wether a process is running."
  [proc]
  (= (get-process-state! proc) "active"))

(defn enable-process!
  "Enables a process."
  [proc]
  (sys/enable-service! (:unit-name proc)))

(defn process-enabled?
  "Enables a process."
  [proc]
  (= (sys/get-service-file-state! (:unit-name proc))
     :enabled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Serialization            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn process->edn
  "Serializes the process `proc` into edn."
  [^process proc]
  (prn-str {:id (:id proc) :name (:process-name proc)
            :config (:ffmpeg-config proc)
            :version 1}))

(defn edn->process!
  "Creates a process from its `edn` serialized version."
  [edn]
  (let [{:keys [name config id]} (clojure.edn/read-string edn)]
    (create-process! name config id)))

(defn processdb-filename
  "Computes the filename of the serialization file for the given process
  `proc`."
  [id]
  (str processdb-directory "/" id ".edn"))

;; TODO: error hangling
(defn save-process!
  "Serializes process `proc` information to a file in the
  [[processdb-directory]]. Returns `true` on success.

  It writes a temporary file first and then move it."
  [proc]
  (info "Writing processdb file for process:" (:process-name proc))
  (let [tempfile
        (File/createTempFile (:process-name proc) "_stream" (File. "/tmp"))]
    (spit tempfile (process->edn proc))
    (trace "Tepmfile is:" tempfile)
    (try+
     (Files/move (.toPath tempfile)
                 (Paths/get (processdb-filename (:id proc))
                            (into-array String []))
                 (into-array CopyOption
                             [(StandardCopyOption/REPLACE_EXISTING)]))
     true
     (catch Object _
       (error  "could not write process file for " (:process-name proc))
       (throw+ {:type ::commander-error
                :detail-type :saving-failed
                :error (:throwable &throw-context)})))))

;; TODO: Global warnings
(defn load-process!
  "Loads a process with the `id` from the processdb directory.
  Returns `nil` if the db file is not found or can't be read and
  the loaded process otherwise."
  [id]
  (if-let [proc-data
           (try+
            (slurp (processdb-filename id))
            (catch Object _
              (error "could not load process file " (processdb-filename id))
              (throw+ {:type ::commander-error
                       :detail-type :loading-failed
                       :error (:throwable &throw-context)})))]
    (edn->process! proc-data)
    nil))

;; TODO: notify of failed ones
(defn load-processes!
  "Loads the serialized processes from the processdb directory by
  iterating over it. Returns an array of processes."
  []
  (reduce (fn [procs file]
            (if-let [id (second (re-matches #"(.*)\.edn" file))]
              (try+ (conj procs (load-process! id))
                    (catch Object _
                      procs))
              procs))
          [] (.list (File. processdb-directory))))

(defn delete-processdb-file!
  "Deletes the processdb file of a process.
  Returns `false` if the file is not found."
  [id]
  (try+
   (io/delete-file (processdb-filename id))
   (catch Object _
     (error "could not delete process file " (processdb-filename id))
     (throw+ {:type ::commander-error
              :detail-type :deleting-failed
              :error (:throwable &throw-context)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 Init                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init! []
  "Initialize the systemd connection and makes the commander API
  operable. Processess will have to be loaded manually."
  (sys/init!))
