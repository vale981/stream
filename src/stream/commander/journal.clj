(ns stream.commander.journal
  (:require [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [clojure.java.shell :refer [sh]]
            [clojure.set :refer [map-invert]]
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

(def ^:private level-strings
  (map-invert levels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Journal Commands          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- map->flags
  "Converts a hashmap {:a 1 :b 2} to command line flags."
  [map]
  (let [flags (reduce (fn [options [flag value]]
                        (if (and value (not (= flag :verbatim)))
                          (conj options
                                (let [name (name flag)]
                                  (if (> (count name) 1)
                                    (str "--" name "=" value)
                                    (str "-" name value))))
                          options))
                      [] map)
        verb (:verbatim map)]
    (if verb
      (conj flags verb)
      flags)))

(defn- run-journalctl-command!
  "Boilerplate to run a journalctl command over the shell.
   Returns the parsed json output as a lazy seq of maps.
   This need not be performant."
  [& {:as options}]
  (let [result
        (apply sh "journalctl" "-a" "--user" "-ojson" (map->flags options))]
    (if (= (:exit result) 0)
      (if (> (count (:out result)) 0)
        (map #(json/read-str % :key-fn (comp keyword str/lower-case))
             (str/split-lines (:out result)))
        [])
      (throw+ {:type ::journalctl-error
               :detail-type ::log-read-error
               :message (:err result)}))))

(defn get-logs!
  "Reads the logs from a systemd user unit specified by its name: `unit`.

  :number
      How many of the latest log entries to read.
      Defaults to 10."
  [unit & {:keys [number priority executable] :or {number 10 executable false}}]
  (let [logs (run-journalctl-command!
              :u unit :p (get level-strings priority)
              :n number :verbatim executable)]
    (map
     (fn [entry]
       {:level (get levels (:priority entry))
        :message (:message entry)})
     logs)))
