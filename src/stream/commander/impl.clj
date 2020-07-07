(ns stream.commander.impl
  (:require [cljstache.core :as template]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [clojure.java.shell :refer [sh]]
            [slingshot.slingshot :refer [throw+]])
  (:import [de.thjom.java.systemd Systemd Manager Systemd$InstanceType
            UnitStateListener]))

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

;; The control commands are implemented through shell calls, because
;; the dbus library is broken and the performance is not important.
;; The commands are implemented synchronousily.

(defn- run-systemd-command! [& commands]
  (let [result (apply sh "systemctl" "--user" "--job-mode=replace" commands)]
    (if (= (:exit result) 0)
      true
      (throw+ {:type ::systemd-error
               :name name :message (:err result)}))))

(defn reload-systemd!
  "Reloads the systemd user instance."
  []
  (. manager reload))

(defn- get-service! [name]
  (. manager getService name))

(defn start-service!
  "Starts the userspace service with the name."
  [name]
  (run-systemd-command! "start" name))

(defn restart-service!
  "Restarts the userspace service with the name."
  [name]
  (run-systemd-command! "restart" name))

(defn stop-service!
  "Stops the userspace service with the name."
  [name]
  (run-systemd-command! "stop" name))

(defn get-service-state!
  "Gets the ActiveState for the service of the name.

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

(defn get-service-file-state!
  "Gets the UnitFileState for the process of the name.
  Returns false if the service file isn't found.

  Refer to
  [the systemd docs](https://www.freedesktop.org/wiki/Software/systemd/dbus/)
  for further information."
  [name]
  (let [state (. (get-service! name) getUnitFileState)]
    (if (> (count state) 0)
      (keyword state)
      false)))

;; TODO: PR to implement in dbus lib.
(defn enable-service!
  "Enables the service with the name. Returns true on success."
  [name]
  (let [result (sh "systemctl" "--user" "enable" name)]
    (if (= (:exit result) 0)
      true
      (throw+ {:type ::systemd-error :message "Service can't be enabled."
               :name name :err (:err result)}))))

(defn disable-service!
  "Disables the service with the name."
  [name]
  (run-systemd-command! "disable" name))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Graveyard              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (comment
;;   (. manager addConsumer ManagerInterface$JobRemoved
;;      (reify DBusSigHandler (handle [this sig] (println sig)))))
