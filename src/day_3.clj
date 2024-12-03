(ns day-3
  (:require [clojure.string :as str])
  (:use [prelude]))

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

(defn compute-part-2
  ([input]
   (compute-part-2 input true))
  ([input enabled?]
   (if (empty? input)
     0
     (case (first input)
       :do (compute-part-2 (rest input) true)
       :don't (compute-part-2 (rest input) false)
       (if-not enabled?
         (compute-part-2 (rest input) enabled?)
         (+ (first input) (compute-part-2 (rest input) enabled?)))))))

(def part-2
  (let [m (re-matcher #"mul\((\d{1,3}),(\d{1,3})\)|(do|don't)\(\)" input)]
    (->> #(re-find m)
         (repeatedly)
         (take-while some?)
         (sequence
           (comp
             (map rest)
             (map #(remove nil? %))
             (map #(case (count %)
                     2 (apply * (map parse-long %))
                     (-> % first keyword)))))
         (compute-part-2))))
