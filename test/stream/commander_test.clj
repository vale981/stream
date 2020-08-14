(ns stream.commander-test
  (:require [stream.commander.processes :as p]
            [stream.commander.api :as api]
            [stream.core :as core]
            [stream.util.logging :as logging]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [slingshot.slingshot :refer [try+]]
            [clojure.core.async
             :as a]))

(core/init!)

(deftest auxiliary-api
  (testing "ffmpeg input string"
    (is (= "rtsp://192.168.1.205:554/axis-media/media.amp"
           (#'api/input-string nil nil "192.168.1.205" 554 "axis-media/media.amp")))
    (is (= "rtsp://cam:pass@192.168.1.205:554/axis-media/media.amp"
           (#'api/input-string "cam" "pass" "192.168.1.205" 554 "axis-media/media.amp"))))

  (testing "unit name sanitizer"
    (is (= "a-b-c-d-" (#'api/sanitize-process-name "a*b C?d.")))))

(deftest ffmpeg-process-management
  (.mkdirs (.getParentFile (clojure.java.io/file api/processdb-directory)))
  ;; NOTE: This creates a failing process.
  (let [config {:cam-ip "0.0.0.0"
                :cam-rtsp-port "554"
                :profile "bla"
                :stream-server "server"
                :stream-key "key"}]
    ;; TODO: proper spec testing
    (testing "getting an ffmpeg command"
      (api/ffmpeg-command config))

    (let [proc (api/create-process!
                "tester" config)]
      (testing "getting the newly created process state"
        (is (not @(p/alive? proc))))

      (testing "starting a process (which will fail)"
        (is (not (:success @(p/start! proc)))))

      (testing "spilling junk into the monitor channel"
        (a/>!! (:monitor proc) "junk"))

      (testing "stopping the process"
        (let [prom (p/stop! proc)]
          (is (= :not-running (:message @prom)))
          (is (not @(p/alive? proc)))))

      (testing "re starting a process"
        (is (not (:success @(p/restart! proc)))))

      (testing "the subscription to the master monitor"
        (let [c (a/chan)]
          (a/sub api/monitor :process-event c)
          (p/start! proc)
          (is (= (:id proc) (:id (a/<!! c))))))

      (testing "deleting the process"
        (is (api/delete-process! proc))
        (is (not (api/get-process! (:id proc))))))

    (testing "creating two processes"
      (api/create-process!
       "tester" config)
      (api/create-process!
       "tester" config)
      (is (= 2 (count @@#'api/processes))))

    (testing "replace process"
      (let [proc (api/create-process! "ghost" config)
            proc-new (api/create-process! "ghost" config (:id proc))]
        (is (not (= proc proc-new)))))

    (testing "generated ids do not collide"
      (doseq [i (range 100)]
        (is (not (api/get-process! (#'api/generate-process-id))))))

    (testing "deleting all processes"
      (api/delete-all-processes!)
      (is (= 0 (count @@#'api/processes))))

    (testing "serialize and load a process"
      (let [proc (api/create-process! "test" config)
            id (:id proc)
            saved (api/save-process! proc)
            loaded (api/load-process! id)
            edn (api/process->edn proc)
            fromedn (api/edn->process! edn)]
        (is (api/process= proc fromedn))
        (is saved)
        (is (api/process= proc loaded))
        (api/delete-all-processes!)))

    (testing "saving a process twice"
      (let [proc (api/create-process! "test" config)]
        (api/save-process! proc)
        (api/save-process! proc)
        (api/delete-all-processes!)))

    (testing "loading a nonexistent process"
      (try+
       (api/load-process! "nonone")
       (is false)
       (catch [:type :stream.commander.api/commander-error
               :detail-type :loading-failed] _
         (is true))))

    (testing "deleting a nonexistent process"
      (try+
       (api/delete-processdb-file! "nonone")
       (is false)
       (catch [:type :stream.commander.api/commander-error
               :detail-type :deleting-failed] _
         (is true))))

    (testing "automatic saving and loading"
      (let [procs [(api/create-process!
                    "tester" config)
                   (api/create-process!
                    "tester2" config)]]
        ;; lets put in a random broken edn file-seq
        (spit (api/processdb-filename "distract") "bla")
        (let [loaded (api/load-processes!)]
          (is (= (count loaded) 2))
          (is (reduce (fn [same proc]
                        (if same
                          (some #(api/process= proc %) procs)
                          false)) true
                      (api/load-processes!)))))
      (is (= 2 (count @@#'api/processes)))
      (api/delete-all-processes!)
      (clojure.java.io/delete-file (api/processdb-filename "distract"))
      (is (= "" (reduce str (.list (clojure.java.io/file api/processdb-directory))))))))
