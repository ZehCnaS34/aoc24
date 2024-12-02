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
    (= 1 (count (into #{} (map (comp pos-int? -) coll (rest coll)))))))

(def part-1
  (as-> input $
        (filter safe? $)
        (count $)))
