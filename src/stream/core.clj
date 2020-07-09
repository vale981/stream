(ns stream.core
  (:gen-class)
  (:require
   [stream.commander.api :as commander]
   [stream.util.logging :as logging]))

(defn init! []
  "Initialize all the modules."
  (logging/init!)
  (commander/init!))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
