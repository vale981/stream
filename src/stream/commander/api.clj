(ns stream.commander.api
  (:require [stream.commander.systemd :as sys]
            [stream.commander.journal :as journal]
            [clojure.string :as string]
            [slingshot.slingshot :refer [throw+]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    Globals, Structures and State    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: specs
;; TODO: sanitize custom flags

(defrecord process
    [id process-name unit-name monitor supervisor ffmpeg-config problems])

(def ^:private processes (ref {}))
(def ^:private dbus-monitor (chan))
(def ^:private master-monitor (chan))

(def monitor (a/pub master-monitor :type))

;; TODO: load from config
(def default-ffmpeg-config
  {:ffmpeg-path "/usr/bin/ffmpeg" :rtsp-transport "tcp" :custom-flags ""
   :audio-bitrate "128k" :audio-channels 1 :audio-sampling-rate 44100
   :rtsp-user nil :rtsp-password nil :buffer-size "100M"})

(def ^:const +default-timeout+ 1000)

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
                                        ;              Monitoring             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-dbus-event
  "Parses a dbus state `update` into an event."
  [update id]
  (condp = (:ActiveState update)
    "failed" {:event :failed
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
    "active" {:event :active}
    "activating" {:event :activating}
    "deactivating" {:event :deactivating}
    "inactive" {:event :inactive}
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
                 :or {timeout +default-timeout+}}]
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

(defn create-process!
  "Creates a process, adds it to the registry and assigns a monitoring
  channel to it. Returns the process."
  [process-name ffmpeg-config]
  (let [id (generate-process-id)
        ffmpeg-config (merge default-ffmpeg-config ffmpeg-config)
        unit-name (str (sanitize-process-name process-name)
                       "-" id)
        path (sys/create-service! unit-name
                                  (ffmpeg-command ffmpeg-config)
                                  (get ffmpeg-config :description
                                       "FFMPEG streaming process, created by `stream`."))
        monitor (sys/create-monitor! unit-name)
        supervisor (attach-supervisor! id (first monitor))
        process (->process id process-name unit-name monitor supervisor ffmpeg-config #{})]
    (info "Creating process with ID:" id)
    (dosync
     (commute processes assoc id process))
    process))

(defn get-process!
  "Get the process with the id."
  [id]
  (get @processes id))

(defn delete-process!
  "Deletes a process from the process map, stops it and deletes the unit file.
  Returns `true` on success, `false` otherwise."
  [proc]
  (debug "Removing process with ID:" (:id proc))
  (let [{:keys [unit-name monitor]} proc
        [monitor close] monitor]
    (close! (:supervisor proc))
    (sys/remove-service! unit-name)
    (close)
    (dosync (commute processes dissoc (:id proc)))
    true))

(defn delete-all-processes! []
  "Deletes all processes."
  (doseq [[_ proc] @processes]
    (delete-process! proc)))

(defn get-process-state!
  "Queries wether a process is running."
  [proc]
  (sys/get-service-state! (:unit-name proc)))

(defn start-process!
  "Starts the service associated to the process `proc` (either id or
  process object). Returns a promise that resolves to event `:failed`
  or `:active` or times out after `timeout` ms."
  ([proc timeout]
   (let [prom
         (wait-for! proc :matcher #(some #{(:event %1)} [:active :failed]))]
     (sys/start-service! (:unit-name proc))
     prom))
  ([proc]
   (start-process! proc +default-timeout+)))

(defn stop-process!
  "Stops the service associated to the process."
  [proc]
  (sys/stop-service! (:unit-name proc)))

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
                                        ;                 Init                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init! []
  "Initialize the systemd connection."
  (sys/init!))
