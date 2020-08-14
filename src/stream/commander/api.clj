(ns stream.commander.api
  (:require [stream.commander.processes :as procs]
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
    [id process-name monitor status control ffmpeg-config problems]
    procs/ControllableProcess
    (control [this] control))

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

(defn get-process!
  "Get the process with the id."
  [id]
  (get @processes id))

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

(defconfig ^{:validate [integer? "Must be an integer."]}
  default-buffer-size 50)

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

(defn diagnose-runtime-error
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

(defn diagnose-start-error
  "Deduces the problem from the error `message` and the process
  information `proc` and returns it.

  The returned codes are:
    "
  [message _]
  (cond
    (string/includes? message "No such file") :ffmpeg-not-found
    :else :unknown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Monitoring             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- put-process-event!
  [id data]
  (a/put! master-monitor {:type :process-event
                          :id id
                          :event data}))

(defn- handle-process-error!
  [event id]
  (when-let [proc (get-process! id)]
    (let [problem
          (case (:detail-type event)
            :nonzero-exit (diagnose-runtime-error (:stderr (:details event)) proc)
            :start-failed (diagnose-start-error (:message (:details event)) proc)
            nil)]

      (put-process-event! id {:type :new-problem
                              :problem problem})

      (dosync
       (alter processes
              (fn [procs]
                (let [proc (get procs id)
                      updated-proc (assoc proc :problems (conj (:problems proc) problem))]
                  (assoc procs id updated-proc))))))))

(defn- handle-status-event!
  "Handles status `event`s from the process with the `id`."
  [id event]
  (case (:type event)
    :error (handle-process-error! event id))
  (put-process-event! id event))

;; TODO: specs
;; TODO: document event types
(defn- attach-supervisor!
  "Attaches a monitor consumer to the `status` channel of a process with the `id`.
  It processes all status messages from the process and forwards them
  to the main monitor if necessary.

  Returns a control channel, whose sole purpose it is to stop the supervisor thread."
  [id status]
  (let [control (chan)]
    (thread
      (trace "Monitoring" id)
      (loop []
        (let [[event channel]
              (alts!! [status control])]
          (condp = channel
            status
            (do
              (handle-status-event! id event)
              (recur))

            control
            (if (not event)
              (do (trace "Stopping monitoring of" id)
                  (put-process-event! id {:event :monitor-deleted}))
              (recur))))))
    control))

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
    [ffmpeg-path
     (str "-rtsp_transport" rtsp-transport)
     (str "-i" (input-string rtsp-user rtsp-password cam-ip cam-rtsp-port profile))
     "-vcodec copy"
     (str "-b:a" audio-bitrate)
     (str "-ac" audio-channels)
     (str "-ar" audio-sampling-rate)
     "-f flv"
     (str "-bufsize" buffer-size)
     (str "-tune film")
     "-loglevel error" (str stream-server "/" stream-key)]))

(defn- sanitize-process-name [name]
  (string/lower-case (string/replace name #"\W" "-")))

(declare delete-processdb-file!)
(defn delete-process!
  "Deletes a process from the process map, stops it and stops monitoring it."
  [proc]
  (info "Removing process with ID:" (:process-name proc))
  (let [{:keys [monitor]} proc]
    (close! monitor)
    (procs/stop! proc)
    (procs/stop-monitor! proc)
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
         [status control] (procs/launch!
                           (ffmpeg-command ffmpeg-config)
                           :stderr-buffer-size default-buffer-size)
         monitor (attach-supervisor! id status)
         process (->process id process-name monitor status control ffmpeg-config #{})]
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
