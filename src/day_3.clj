(ns day-3
  (:require [clojure.string :as str]
            [clojure.test :as t])
  (:use [prelude]))

(defn input
  [pattern]
  (let [m (->> (slurp-problem 3 1)
               (str/join "")
               (re-matcher pattern)
               (binding [*test* false]))]
    (->> #(re-find m)
         (repeatedly)
         (take-while some?))))

(def part-1
  (->> (input #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (comp #(apply * %) #(map parse-long %) rest))
       (reduce +)))

(t/is (= 166357705 part-1))

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
  (->> (input #"mul\((\d{1,3}),(\d{1,3})\)|(do|don't)\(\)")
       (sequence
         (comp
           (map rest)
           (map #(remove nil? %))
           (map #(case (count %)
                   2 (->> % (map parse-long) (apply *))
                   (-> % (first) (keyword))))))
       (compute-part-2)))

(t/is (= 88811886 part-2))
