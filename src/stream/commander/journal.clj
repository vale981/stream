(ns stream.commander.journal
  (:require [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [clojure.java.shell :refer [sh]]
            [clojure.data.json :as json]
            [slingshot.slingshot :refer [throw+]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       Parameters and Constants      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:doc "The journal loglevels."}
  levels {"0" :emerg,
          "1" :alert,
          "2" :crit,
          "3" :err,
          "4" :warning,
          "5" :notice,
          "6" :info,
          "7" :debug})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Journal Commands          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- map->flags
  "Converts a hashmap {:a 1 :b 2} to command line flags."
  [map]
  (reduce (fn [options [flag value]]
            (conj options
                  (let [name (name flag)]
                    (if (> (count name) 1)
                      (str "--" name "=" value)
                      (str "-" name value)))))
          [] map))

(defn- run-journalctl-command!
  "Boilerplate to run a journalctl command over the shell.
   Returns the parsed json output as a lazy seq of maps.
   This need not be performant."
  [& {:as options}]
  (let [result
        (apply sh "journalctl" "--user" "-ojson" (map->flags options))]
    (if (= (:exit result) 0)
      (map #(json/read-str % :key-fn (comp keyword str/lower-case))
           (str/split-lines (:out result)))
      (throw+ {:type ::journalctl-error
               :detail-type ::command-error
               :message (:err result)}))))

(defn read-logs!
  "Reads the logs from a systemd user unit specified by its name: `unit`.

   :number
       How many of the latest log entries to read."
  [unit & {:keys [number] :or {number 10}}]
  (let [logs (run-journalctl-command! :u unit :n number)]
    (map
     (fn [entry]
       {:level (get levels (:priority entry))
        :message (:message entry)})
     logs)))
