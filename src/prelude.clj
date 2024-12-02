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

(defn parse-numbers
  [input]
  (map
    (comp
      #(mapv parse-long %)
      #(str/split % #"\s+")) input))

(defn counts
  [list]
  (reduce
    (fn [counts item]
      (update counts item (fnil inc 0)))
    {}
    list))