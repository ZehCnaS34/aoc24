(ns day-3
  (:use [prelude])
  (:require [clojure.string :as str]))

(def input
  (->> (binding [*test* false]
        (slurp-problem 3 1))
      (str/join "")))

(def part-1
  (let [m (re-matcher #"mul\((\d{1,3}),(\d{1,3})\)" input)]
    (->> #(re-find m)
         (repeatedly)
         (take-while some?)
         (map (comp #(apply * %) #(map parse-long %) rest))
         (reduce +))))
