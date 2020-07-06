(ns stream.commander-test
  (:require [stream.commander.impl :as impl]
            [stream.commander.api :as api]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(deftest unit-files
  (testing "The rendering."
    (is (#'impl/render-unit-file "test" "test" "test")))

  (let [name (str (java.util.UUID/randomUUID))]
    (testing "Writing a unit file."
      (let [file (io/as-file
                  (impl/create-unit-file! name
                                          "test" "test" "test"))]
        (is (.exists file))))

    (testing "Deleting a unit file"
      (impl/remove-unit-file! name)
      (is (not (.exists (io/as-file (impl/get-unit-path name))))))))

(deftest systemd-services
  (let [name (str (java.util.UUID/randomUUID))
        unit-path (impl/create-unit-file! name
                                          "cat /dev/zero" "test service")]
    (testing "loading the service"
      (impl/reload-systemd!)
      (is (= :loaded (impl/get-service-load-state! name))))

    (testing "starting the service"
      (impl/start-service! name)
      (is (= :active (impl/get-service-load-state! name))))

    (testing "stopping the service"
      (impl/stop-service! name)
      (is (= :inactive (impl/get-service-load-state! name))))

    (testing "enabling the service"
      (impl/enable-service! name)
      (println "==============>" (impl/get-service-file-state! name))
      (is (= :enabled (impl/get-service-file-state! name))))

    (testing "disable the service"
      (impl/disable-service! name)
      (is (= :disabled (impl/get-service-file-state! name))))

    (testing "removing the service"
      (impl/remove-service! name)
      (is (= :not-found (impl/get-service-load-state! name))))))
