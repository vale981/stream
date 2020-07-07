(ns stream.commander-test
  (:require [stream.commander.systemd :as systemd]
            [stream.commander.api :as api]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

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

(deftest systemd-services
  (let [name (str (java.util.UUID/randomUUID))
        unit-path (systemd/create-unit-file! name
                                          "cat /dev/zero" "test service")]
    (testing "loading the service"
      (systemd/reload-systemd!)
      (is (= :loaded (systemd/get-service-load-state! name))))

    (testing "starting the service"
      (systemd/start-service! name)
      (is (= :active (systemd/get-service-state! name))))

    (testing "stopping the service"
      (systemd/stop-service! name)
      (is (= :inactive (systemd/get-service-state! name))))

    (testing "enabling the service"
      (systemd/enable-service! name)
      (is (= :enabled (systemd/get-service-file-state! name))))

    (testing "disable the service"
      (systemd/disable-service! name)
      (is (= :disabled (systemd/get-service-file-state! name))))

    (testing "removing the service"
      (systemd/remove-service! name)
      (is (= :not-found (systemd/get-service-load-state! name))))))
