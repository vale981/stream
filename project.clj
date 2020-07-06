(defproject stream "0.1.0-SNAPSHOT"
  :description "the Simple Transmission Emitter And Monitor"
  :url "https://github.com/vale981/stream"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.github.thjomnx/java-systemd "1.1.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [cljstache/cljstache "2.0.6"]
                 [org.clojure/core.async "1.2.603"]
                 [slingshot "0.12.2"]]
  :main ^:skip-aot stream.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
