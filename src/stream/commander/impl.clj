(ns stream.commander.impl
  (:require [cljstache.core :as template]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         Unit file Management        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- render-unit-file
  "Renders the unit file from a template.
  The template can be found at:
  resources/commander/templates/unit.mustache"
  [command description target]
  (template/render-resource "commander/templates/unit.mustache"
                            {:description description
                             :command command
                             :target target}))

(defn create-unit-file!
  "Creates or overwrites a service unit file in the appropriate
  directory and returns the file path."
  [name command description target]
  (let [unit-contents (render-unit-file command description target)
        path (str (System/getProperty "user.home") "/.config/systemd/user/" name ".service")
        file (io/as-file path)]
    (.mkdirs (.getParentFile file))
    (spit path unit-contents)
    (debug "Wrote a unit file to:" path)
    path))
