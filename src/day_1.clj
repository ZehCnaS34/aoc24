(ns day-1
  (:require [clojure.string :as str])
  (:use [prelude]))

(def part-1
  (as->
    (slurp-problem 1 1)
    $
    (map
      (comp
        #(mapv parse-long %)
        #(str/split % #"\s+")) $)
    (reduce (fn [[left right] [l r]]
              [(conj left l)
               (conj right r)])
            [[] []]
            $)
    (update $ 0 sort)
    (update $ 1 sort)
    (map (comp abs -) (first $) (second $))
    (reduce + $)))
