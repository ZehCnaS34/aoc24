(ns day-1
  (:require [clojure.string :as str])
  (:use [prelude]))

(def input
  (as->
    (slurp-problem 1 1)
    $
    (map (comp #(mapv parse-long %)
               #(str/split % #"\s+")) $)
    (reduce (fn [[left right] [l r]]
              [(conj left l)
               (conj right r)])
            [[] []]
            $)))

(def part-1
  (as->
    input
    $
    (update $ 0 sort)
    (update $ 1 sort)
    (map (comp abs -) (first $) (second $))
    (reduce + $)))

(def part-2
  (let [counts (counts (second input))]
    (as->
      (first input)
      $
      (map #(* % (counts % 0)) $)
      (reduce + 0 $))))
