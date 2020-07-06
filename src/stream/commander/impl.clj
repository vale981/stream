(ns stream.commander.impl
  (:require [cljstache.core :as template]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]])
  (:import [de.thjom.java.systemd Systemd Manager Systemd$InstanceType UnitStateListener]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;          Systemd Instances          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are instanciated toplevel and are allowed to crash the
;; program if not successfull.

(def ^:private systemd (. Systemd get Systemd$InstanceType/USER))
(def ^:private manager (. systemd getManager))

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

(defn get-unit-path [name]
  (str (System/getProperty "user.home") "/.config/systemd/user/" name ".service"))

(defn create-unit-file!
  "Creates or overwrites a service unit file in the appropriate
  directory and returns the file path."
  ([name command description]
   (create-unit-file! name command description "default.target"))
  ([name command description target]
   (let [unit-contents (render-unit-file command description target)
         path (get-unit-path name)
         file (io/as-file path)]
     (.mkdirs (.getParentFile file))
     (spit path unit-contents)
     (debug "Wrote a unit file to:" path)
     path)))


(defn remove-unit-file!
  "Removes the unit file with the given name. Returns true if file was
  deleted."
  [name]
  (let [path (get-unit-path name)]
    (debug "Deleting a unit file:" path)
    (.delete (io/as-file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Systemd Handling          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now these work with the service name. May be generalized to a
;; record some time.

(defn reload-systemd!
  "Reloads the systemd user instance."
  []
  (. manager reload))

(defn- get-service! [name]
  (. manager getService name))

(defn start-service!
  "Starts the userspace service with the name."
  [name]
  (. (get-service! name) start "replace"))

(defn restart-service!
  "Restarts the userspace service with the name."
  [name]
  (. (get-service! name) restart "replace"))

(defn stop-service!
  "Stops the userspace service with the name."
  [name]
  (. (get-service! name) stop "replace"))

(defn get-service-status!
  "Gets the ActiveState for the process of the name.

  Refer to
  [the systemd docs](https://www.freedesktop.org/wiki/Software/systemd/dbus/)
  for further information."
  [name]
  (keyword (. (get-service! name) getActiveState)))

(defn get-service-load-state!
  "Gets the LoadState for the process of the name.

  Refer to
  [the systemd docs](https://www.freedesktop.org/wiki/Software/systemd/dbus/)
  for further information."
  [name]
  (keyword (. (get-service! name) getLoadState)))

(defn create-service!
  "Creates a unit file and reloads systemd. See `create-unit-file`."
  [name command description target]
  (create-unit-file! name command description target)
  (reload-systemd!))

(defn remove-service!
  "Stops the service, removes the unit file and reloads systemd."
  [name]
  (stop-service! name)
  (remove-unit-file! name)
  (reload-systemd!))
