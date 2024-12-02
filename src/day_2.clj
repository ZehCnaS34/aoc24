(ns day-2
  (:use [prelude]))

(def input
  (-> (binding [*test* false]
        (slurp-problem 2 1))
      (parse-numbers)
      (vec)))

(defn safe?
  [coll]
  (and
    (every? #(<= 1 % 3) (map (comp abs -) coll (rest coll)))
    (= 1 (count (into #{} (map (comp pos-int? -) coll (rest coll)))))
    coll))

(defn safe-after-edit?
  ([coll]
   (or (safe? coll)
       (->> (count coll)
            (range)
            (map #(concat (take % coll) (drop (inc %) coll)))
            (some safe?)))))

(def part-1
  (->> input
       (filter safe?)
       (count)))

(def part-2
  (->> input
       (filter safe-after-edit?)
       (count)))

