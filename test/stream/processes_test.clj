(ns stream.processes-test
  (:require  [clojure.test :refer :all]
             [stream.commander.processes :refer :all]
             [slingshot.slingshot :refer [try+]]
             [clojure.core.async
              :as a
              :refer [>! <! >!! <!! go chan buffer close! thread
                      alts! alts!! timeout]]))


(deftest invalid-command
  (testing "creating a bogus command"
    (let [[status control] (launch! ["bogus"])]
      (is (not (:success @(start! control))))
      (is (= :start-failed (:detail-type (<!! status)))))))

(deftest basics
  (let [[status control]
        (launch! ["/bin/sh" "resources/test/test-executables/simple.sh" "11"]
                 :stderr-buffer-size 2)]
    (testing "if waiting for a nont started process returns false"
      (is (= false @(wait-for control))))

    (dotimes [round 3]                  ; for sanity
      (testing (str "starting the process #" round)
        (is (:success @(start! control)))
        (is (= :started (:event (<!! status))))
        (is (= 11 @(wait-for control)))
        (is (= 11 @(wait-for control))) ; twice
        (let [stop-evt (<!! status)]
          (is (= :stopped (:event stop-evt)))
          (is (= "stderr" (-> stop-evt :stderr first)))
          (is (= 2 (-> stop-evt :stderr count)))
          (is (= 11 (:code stop-evt))))
        (let [err (<!! status)]
          (is (= :error (:type err)))
          (is (= 11 (-> err :details :code))))))

    (testing "spilling junk into the control channel"
      (>!! control "junk"))

    (testing "destroying the monitor"
      (is @(stop-monitor! control))
      (is (not (>!! status 0)))
      (is (not (>!! control 0))))))

(deftest long-running
  (let [[status control]
        (launch! ["/bin/sh" "resources/test/test-executables/long-running.sh" "11"]
                 :stderr-buffer-size 2)]

    (testing "starting the daemon"
      (is (:success @(start! control))))

    (testing "checking if the process is alive"
      (is @(alive? control)))

    (testing "restarting the daemon"
      (is (:success @(start! control))))

    (testing "stopping the daemon"
      (is (:success @(stop! control)))
      (is (not @(alive? control))))

    (testing "stopping the daemon, again"
      (is (= :not-running
             (:message @(stop! control)))))

    (testing "destroying the monitor"
      (is @(stop-monitor! control))
      (is (not (>!! status 0)))
      (is (not (>!! control 0))))))
