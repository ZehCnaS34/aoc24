(ns day-2
  (:use [prelude]))

(def input
  (as-> (binding [*test* false] (slurp-problem 2 1)) $
        (parse-numbers $)
        (vec $)))

(defn safe?
  [coll]
  (and
    (every? #(<= 1 % 3) (map (comp abs -) coll (rest coll)))
    (= 1 (count (into #{} (map (comp pos-int? -) coll (rest coll)))))
    coll))

(defn safe-after-edit?
  ([coll]
   (some
     safe?
     (for [i (range (count coll))
           :let [left (take i coll)
                 right (drop (inc i) coll)]]
       (concat left right)))))

(def part-1
  (as-> input $
        (filter safe? $)
        (count $)))

(def part-2
  (as-> input $
        (filter safe-after-edit? $)
        (count $)))
