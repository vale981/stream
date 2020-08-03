(ns stream.commander-test
  (:require [stream.commander.systemd :as systemd]
            [stream.commander.journal :as journal]
            [stream.commander.api :as api]
            [stream.core :as core]
            [stream.util.logging :as logging]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [slingshot.slingshot :refer [try+]]
            [clojure.core.async
             :as a]))

(core/init!)

(deftest unit-files
  (testing "The rendering."
    (is (#'systemd/render-unit-file "test" "test" "test")))

  (let [name (str (java.util.UUID/randomUUID))]
    (testing "Writing a unit file."
      (let [file (io/as-file
                  (systemd/create-unit-file! name
                                             "test" "test" "test"))]
        (is (.exists file))))

    (testing "Deleting a unit file"
      (systemd/remove-unit-file! name)
      (is (not (.exists (io/as-file (systemd/get-unit-path name))))))))

(deftest journal-utilities
  (testing "map to flags"
    (is (= (#'journal/map->flags {:t 1 :u 2})
           ["-t1" "-u2"]))
    (is (= (#'journal/map->flags {:t 1 :u 2 :test 11})
           ["-t1" "-u2" "--test=11"]))
    (is (= (#'journal/map->flags {:t 1 :u 2 :test 11 :verbatim "hi"})
           ["-t1" "-u2" "--test=11" "hi"]))))

(deftest journal
  (testing "returns empty seq if there are no logs"
    (is (empty? (journal/get-logs! "nonexistent"))))
  (testing "throws upon error in journalctl"
    (try+
     (#'journal/run-journalctl-command! :uuuu "bad")
     (is false "No exception thrown.")
     (catch [:type :stream.commander.journal/journalctl-error] _
       (is true))
     (catch Object _
       (is false "Wrong exception thrown.")))))

(def script
  (str "/bin/bash -c \""
       "echo test; "
       ">&2 echo error;"
       "cat /dev/zero"
       "\""))

(deftest systemd-services
  (let [name (str "testing-" (java.util.UUID/randomUUID))
        unit-path
        (systemd/create-unit-file! name
                                   script "test service")]
    (testing "loading the service"
      (systemd/reload-systemd!)
      (is (= :loaded (systemd/get-service-load-state! name))))

    (testing "starting the service"
      (systemd/start-service! name)
      (while (= :activating (systemd/get-service-state! name))
        (a/<!! (a/timeout 1000)))
      (is (= :active (systemd/get-service-state! name))))

    (testing "stopping the service"
      (systemd/stop-service! name)
      (while (= :deactivating (systemd/get-service-state! name))
        (a/<!! (a/timeout 1000)))
      (is (= :inactive (systemd/get-service-state! name))))

    (testing "reading the logs"
      (let [logs (journal/get-logs! name)]
        (is (= (:message (second logs)) "test"))
        (is (= (:message (nth logs 2)) "error"))))

    (testing "restarting the service"
      (systemd/start-service! name)
      (systemd/restart-service! name)
      (while (some #(= % (systemd/get-service-state! name))
                   [:activating :deactivating :inactive])
        (a/<!! (a/timeout 1000)))
      (is (= :active (systemd/get-service-state! name))))

    (testing "enabling the service"
      (systemd/enable-service! name)
      (is (= :enabled (systemd/get-service-file-state! name))))

    (testing "disable the service"
      (systemd/disable-service! name)
      (is (= :disabled (systemd/get-service-file-state! name))))

    (testing "removing the service"
      (systemd/remove-service! name)
      (is (= :not-found (systemd/get-service-load-state! name))))

    (testing "creating a service automatically"
      (systemd/create-service! name script "test service")
      (is (= :loaded (systemd/get-service-load-state! name))))

    (let [[channel close] (systemd/create-monitor! name)]
      (testing "detecting activity"
        (systemd/restart-service! name)
        (is (a/<!! channel) "No value in channel!")
        (while (a/poll! channel))
        (close)
        (is (not (a/put! channel "hi")))))

    (testing "failing systemd command"
      (systemd/remove-service! name)
      (try+
       (systemd/start-service! "non-existend")
       (is false "No exception thrown.")
       (catch [:type :stream.commander.systemd/systemd-error] _
         (is true))
       (catch Object _
         (is false "Wrong exception thrown."))))

    (testing "creating service with digit as first char"
      (try+
       (systemd/create-service! "1234" script "test service")
       (catch [:type :stream.commander.systemd/systemd-error] _
         (is true))
       (catch Object _
         (is false "Wrong exception thrown."))))

    (testing "getting file state state of nonexisting service"
      (is (not (systemd/get-service-file-state!
              (str "does_not_exist"
                   (java.util.UUID/randomUUID))))))))

(deftest auxiliary-api
  (testing "ffmpeg input string"
    (is (= "rtsp://192.168.1.205:554/axis-media/media.amp"
           (#'api/input-string nil nil "192.168.1.205" 554 "axis-media/media.amp")))
    (is (= "rtsp://cam:pass@192.168.1.205:554/axis-media/media.amp"
           (#'api/input-string "cam" "pass" "192.168.1.205" 554 "axis-media/media.amp"))))

  (testing "unit name sanitizer"
    (is (= "a-b-c-d-" (#'api/sanitize-process-name "a*b C?d.")))))

(deftest ffmpeg-process-management
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
      (testing "creating ffmpeg process the high-level way"
        (is (= :loaded (systemd/get-service-load-state!
                        (:unit-name proc)))))

      (testing "getting the newly created process state"
        (is :loaded (api/get-process-state! proc)))

      (testing "starting a process"
        (is (not (= :timeout (:event @(api/start-process! proc))))))

      (testing "waiting for the process to start"
        (let  [prom (api/wait-for!
                     proc
                     :event :active :timeout 10000)]
          (api/start-process! proc)
          (is (not (= :timeout @prom)))))

      (testing "waiting for the process to fail"
        (let  [prom (api/wait-for!
                     proc
                     :event :failed
                     :timeout 100000)]
          (api/start-process! proc)
          (is (not (= :timeout @prom)))))

      (testing "waiting for the process to activate or fail"
        (let  [prom (api/wait-for!
                     proc
                     :matcher #(or (= (:event %1) :active)
                                  (= (:event %1) :failed))
                     :timeout 1000)]
          (api/start-process! proc)
          (is (not (= :timeout @prom)))))

      (testing "spilling junk into the control channel"
        (a/>!! (:supervisor proc) "junk"))

      (testing "spilling junk into the monitor channel"
        (a/>!! (first (:monitor proc)) "junk"))

      (testing "waiting for a timeout"
        (let  [prom (api/wait-for!
                     proc
                     :event :one
                     :timeout 100)
               prom1 (api/wait-for!
                      proc
                      :matcher #(and % false)
                      :timeout 100)]
          (is (= :timeout @prom))
          (is (= :timeout @prom1))))

      (testing "stopping the process"
        @(api/start-process! proc)
        (let [prom (api/stop-process! proc)]
          (is (not (= :timeout @prom)))
          (is (not (api/process-running? proc)))))

      (testing "re starting a process"
        (is (not (= :timeout (:event @(api/restart-process! proc))))))

      (testing "enabling the process"
        (api/enable-process! proc)
        (is (api/process-enabled? proc)))

      (testing "the subscription to the master monitor"
        (let [c (a/chan)]
          (a/sub api/monitor :process-event c)
          (api/start-process! proc)
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

    (testing "generated ids do not collide"
      (doseq [i (range 100)]
        (is (not (api/get-process! (#'api/generate-process-id))))))

    (testing "deleting all processes"
      (api/delete-all-processes!)
      (is (= 0 (count @@#'api/processes))))))
