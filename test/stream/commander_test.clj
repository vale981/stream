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
        (is (.exists file))
        (.delete file)))))
