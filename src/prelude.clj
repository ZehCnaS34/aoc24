(ns prelude
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def ^:dynamic *test* false)

(defn slurp-problem
  [day part]
  (as-> (format "day-%s-%s.txt" day part) $
        (if *test* (str $ ".test") $)
        (jio/resource $)
        (slurp $)
        (str/split $ #"\n")))
