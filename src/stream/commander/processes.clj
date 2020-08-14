(ns stream.commander.processes
  "Basic process monitoring and control.

  Builds on `java.lang.Process` and exposes a nicer `core.async` api.
  This implementation lacks in generality and is taylored precicely to
  the needs here."
  (:require
   [slingshot.slingshot :refer [throw+]]
   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]
   [clojure.core.async
    :as a
    :refer [>! <! >!! <!! go chan buffer close! thread
            alts! alts!! timeout]])
  (:import [java.lang ProcessBuilder Process]
           [java.io BufferedReader InputStreamReader
            IOException File]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Messages              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: do that with specs

(defn- error-status
  [message detail-type & {:as details}]
  {:type :error
   :message message
   :detail-type detail-type
   :details details})

(def ^:const started-message
  {:type :event
   :event :started})

(defn- stopped-message
  [event stderr]
  {:type :event
   :event :stopped
   :code event :stderr
   (loop [lines []]
     (if-let [line (<!! stderr)]
       (recur (cons line lines))
       lines))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      Process Interface Helpers      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- stop-proc!
  "Stops the given process."
  [^Process proc]
  (.destroy proc)
  (.waitFor proc))

(defn- make-exit-chan
  "Waits for the process to finish and returns a channel which will
  contain the exit code."
  [^Process proc]
  (thread
    (.waitFor proc)))

(defn- handle-stop!
  "Handles the `:stop` command."
  [proc prom]
  (deliver
   prom
   {:success :true
    :message (if (and proc (.isAlive proc))
     (stop-proc! proc)
     :not-running)}))

(defn- make-stderr-channel!
  "Creates a channel from a `proc` with a sliding buffer of
  `size` (lines) that will contain the stdout messages."
  [^Process proc size]
  (let [out (chan (a/sliding-buffer size))
        stream (.getErrorStream proc)
        reader (-> stream InputStreamReader. BufferedReader.)]
    (thread
      (loop []
        (if-let [line (.readLine reader)]
          (do
            (a/put! out line)
            (recur))
          (do
            (trace "End of stderr stream.")
            (close! out)))))
    out))

(defn start-process!
  "Given a `builder` (instance of `ProcessBuilder`) it will start the
  process and return it."
  [^ProcessBuilder builder]
  (try [(.start builder) nil]
       (catch IOException e
         (error "Could not start process" (.command builder)
                "Got error" (.getMessage e))
         [nil (.getMessage e)])))

(defn- handle-start!
  "Handles the `:start` command."
  [{:keys [proc] :as state} status
   prom ^ProcessBuilder builder {:keys [stderr-buffer-size]}]
  (debug "Starting" (.command builder) stderr-buffer-size)
  (if proc
    (do (deliver prom {:success true}) state)
    (let [[process err] (start-process! builder)
          exit-chan (when process
                      (make-exit-chan process))]
      (if process
        (do (deliver prom {:success true})
            (a/put! status started-message)
            {:proc process :exit-chan exit-chan
             :stderr (make-stderr-channel! process stderr-buffer-size)})
        (do
          (a/put! status (error-status err
                                       :start-failed
                                       :stderr err))
          (deliver prom {:success false :error err})
          nil)))))

(defn- handle-restart!
  "Handles the `:restart` command."
  [{:keys [proc] :as state} status
   prom ^ProcessBuilder builder options]
  (if proc
    (stop-proc! proc))
  (handle-start! state status prom builder options))

(defn- handle-alive
  "Handles the `:alive?` command."
  [proc prom]
  (deliver prom
           (if proc
             (.isAlive proc)
             false)))

(defn- handle-wait-for
  "Handles the `:wait-for` command."
  [proc dead-proc prom]
  (thread
    (deliver
     prom
     (if proc
       (.waitFor proc)
       (if dead-proc
         (try (.exitValue dead-proc)
              (catch java.lang.IllegalStateException _
                false))
         false)))))

(defn- handle-stop-monitor!
  "Handles the `:stop-monitor` command."
  [status control prom]
  (close! status)
  (close! control)
  (deliver prom true))

(defn- handle-process-exit!
  "Handles the process exit."
  [status code stderr]
  (a/put!
   status
   (stopped-message code stderr))
  (when (not (= code 0))
    (a/put!
     status
     (error-status
      (str "Nonzero exit code! (" code ")")
      :nonzero-exit
      :stderr stderr
      :code code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;     Main Process Monitor Thread     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Buffer Size config?
;; TODO: the stream handling is pretty rudimentary!

(defn monitor-and-control-proc!
  "Montitors a process `proc` created using the process `builder`, puts
  status updates into `status` channel and listens for commands on the
  `control` channel.

  Commands take the form `{:command :something, :promise
  optional-promise ...}`"
  [^ProcessBuilder builder status control
   & {:keys [stderr-buffer-size]
      :or {stderr-buffer-size 10}
      :as options}]
  (thread
    (debug "Starting monitoring of" (.command builder))
    (loop [{:keys [proc exit-chan stderr dead-proc] :as state} nil]
      (let [chans (if exit-chan [control exit-chan] [control])
            [event c] (alts!! chans)]
        (trace "Event" event)
        (condp = c
          control
          (let [{:keys [command prom]} event]
            (condp = command
              :stop (do (handle-stop! proc prom)
                        (recur state))

              :start (recur (handle-start!
                             state status prom builder
                             {:stderr-buffer-size stderr-buffer-size}))

              :restart (recur (handle-restart!
                               state status prom builder
                               {:stderr-buffer-size stderr-buffer-size}))

              :alive? (do (handle-alive proc prom)
                          (recur state))

              :wait-for (do (handle-wait-for proc dead-proc prom)
                            (recur state))

              :stop-monitor (handle-stop-monitor! status control prom)

              ;; default -> ignore
              (do
                (when prom
                  (resolve prom :not-implemented))
                (recur state))))

          exit-chan
          (do
            (debug "Process" (.command builder) "stopped with exit code" event)
            (handle-process-exit! status event stderr)
            (recur {:dead-proc proc})))))))


(defprotocol ControllableProcess
  "A structure that contains a control channel for a process."
  (control [this] "Gives the control channel."))

(defmacro encapsulate-command
  [name doc command]
  `(defn ~name
     ~doc
     [proc#]
     (let [prom# (promise)
           control# (if (satisfies? ControllableProcess proc#)
                      (control proc#)
                      proc#)]
       (a/put! control# {:command ~command :prom prom#})
       prom#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Public Interface          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(encapsulate-command
 start! "Starts the process through the control channel." :start)

(encapsulate-command
 restart! "Restarts the process through the control channel." :restart)

(encapsulate-command
 stop! "Stops the process through the control channel." :stop)

(encapsulate-command
 alive? "Checks if the process is alive through the control channel." :alive?)

(encapsulate-command wait-for "Returns a promise that is resolved to
 the process' exit value once it is finished or to false if there
 is/was no process running." :wait-for)

(encapsulate-command
 stop-monitor! "Stops the process monitoring control channel." :stop-monitor)

(defn launch!
  "Creates a process from the `command` (command and arguments as
  separate arguments) via `java.lang.ProcessBuiler` and attaches a
  monitor channel. Returns a status and a control channel. Still has
  to be started.

  For the `options` see [[monitor-and-control-proc!]]."
  [command & options]
  (println command)
  (let [status (chan)
        control (chan)]
    (let [command-list (into-array String command)
          builder (ProcessBuilder. command-list)]
      ;; FIXME: blackholing works for now, but this needs to be nicer
      (.redirectOutput builder (File. "/dev/null"))
      (apply monitor-and-control-proc! builder status control options)
      [status control])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Snippets              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (def p (launch! ["ffmpeg"] :stderr-buffer-size 1))
  (def c (second p))
  (def s (first p))
  (def prom (promise))
  (>!! c {:command :start :prom prom})
  (def prom (promise))
  (>!! c {:command :start :prom prom}))
