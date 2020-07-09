(ns stream.util.logging
  (:require [taoensso.timbre :refer :all]
            [clojure.string :as str]
            [taoensso.encore :as enc :refer [have have? qb]]))

(defn color-output-fn
  "Default (fn [data]) -> string output fn.
  Use`(partial default-output-fn <opts-map>)` to modify default opts."
  ([     data] (color-output-fn nil data))
  ([opts data] ; For partials
   (let [{:keys [no-stacktrace? stacktrace-fonts]} opts
         {:keys [level ?err #_vargs msg_ ?ns-str ?file hostname_
                 timestamp_ ?line]} data
         colors {:debug :cyan, :info :green, :warn :yellow
                 :error :red, :fatal :purple, :report :blue}
         level-str (str/lower-case (name level))]
     (str
      (color-str :white (force timestamp_))
      " - "
      ""
      (if-let [color (colors level)]
        (color-str color level-str)
        level-str)
      ": "
      (color-str :black (force msg_))
      (if (= level :debug)
        (str " - [" (or ?ns-str ?file "?") ":" (or ?line "?") "]"))
      (when-not no-stacktrace?
        (when-let [err ?err]
          (str enc/system-newline (stacktrace err opts))))))))

(let [colors {:debug :cyan, :info :green, :warn :yellow, :error :red, :fatal :purple, :report :blue}]
  (merge-config!
   {:level :debug
    :output-fn color-output-fn}))
