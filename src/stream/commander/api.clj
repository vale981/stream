(ns stream.commander.api
  (:require [stream.commander.systemd :as sys]
            [clojure.string :as string]
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
    [id process-name unit-name monitor ffmpeg-config problems])

(def ^:private processes (ref {}))

(defn- generate-process-id
  []
  (loop []
    (let [id (str (java.util.UUID/randomUUID))]
      (if (contains? @processes id)
        (recur)
        id))))

;; TODO: load from config
(def default-ffmpeg-config
  {:ffmpeg-path "ffmpeg" :rtsp-transport "tcp" :custom-flags ""
   :audio-bitrate "128k" :audio-channels 1 :audio-sampling-rate 44100
   :rtsp-user nil :rtsp-password nil :buffer-size "100M"})

(defn- input-string
  [user pass ip port profile]
  (str "rtsp://"
       (if (and user pass) (str user ":" pass "@"))
       ip ":" port "/" profile))

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
          buffer-size]} (merge default-ffmpeg-config config)]
    (str ffmpeg-path
         " -rtsp_transport " rtsp-transport
         " -i " (input-string rtsp-user rtsp-password cam-ip cam-rtsp-port profile)
         " -vcodec copy -b:a " audio-bitrate
         " -ac " audio-channels
         " -ar " audio-sampling-rate
         " -f flv -bufsize " buffer-size
         " -tune film " stream-server "/" stream-key)))

(defn- sanitize-process-name [name]
  (string/lower-case (string/replace name #"\W" "-")))

(defn create-process!
  "Creates a process, adds it to the registry and assigns a monitoring
  channel to it. Returns the process."
  [process-name ffmpeg-config]
  (let [id (generate-process-id)
        unit-name (str (sanitize-process-name process-name)
                       "-" id)
        path (sys/create-service! unit-name
                                  (ffmpeg-command ffmpeg-config)
                                  (get ffmpeg-config :description
                                       "FFMPEG streaming process, created by `stream`."))
        monitor (sys/create-monitor! unit-name)
        process (->process id process-name unit-name monitor ffmpeg-config #{})]
    (debug "Creating process with ID:" id)
    (dosync
     (commute processes assoc id process))
    process))

(defn delete-process!
  "Deletes a process from the process map, stops it and deletes the unit file.
  Returns `true` on success, `false` otherwise."
  [id]
  (debug "Removing process with ID:" id)
  (if-let [proc (get @processes id)]
    (let [{:keys [unit-name monitor]} proc
          [_ close] monitor]
      (sys/remove-service! unit-name)
      (println close)
      (close)
      (dosync (commute processes dissoc id))
      true)
    false))

(defn delete-all-processes! []
  "Deletes all processes."
  (doseq [[id _] @processes]
    (delete-process! id)))

(defn get-process!
  "Get the process with the id."
  [id]
  (get @processes id))
