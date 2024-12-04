(ns day-4
  (:require [clojure.string :as str]
            [clojure.test :as t])
  (:use [prelude]))

(def input
  (->>
    (binding [*test* false]
      (slurp-problem 4 1))
    (mapv vec)))

(def north #(update % 0 dec))
(def south #(update % 0 inc))
(def east #(update % 1 inc))
(def west #(update % 1 dec))

(def directions
  (for [longitude [west identity east]
        latitude [north identity south]
        :when (not (= identity longitude latitude))]
    (comp longitude latitude)))

(defn path [pos direction]
  (cons pos (lazy-seq (path (direction pos) direction))))

(defn bounds
  [grid]
  {:rows (count grid)
   :cols (count (first grid))})

(defn in-bounds? [{:keys [rows cols]} [row col]]
  (and (<= 0 row (dec rows))
       (<= 0 col (dec cols))))

(defn read-word
  ([grid pos direction]
   (read-word grid pos direction 4))
  ([grid pos direction amount]
   (->> (path pos direction)
        (take-while (partial in-bounds? (bounds grid)))
        (take amount)
        (map #(get-in grid %))
        (str/join))))

(defn xmas-count
  [grid pos]
  (->> (for [d directions] (read-word grid pos d))
       (filter (partial = "XMAS"))
       (count)))

(def part-1
  (reduce
    +
    (for [row (range (count input))
          col (range (count (first input)))
          :when (= \X (get-in input [row col]))]
      (xmas-count input [row col]))))


;; EW
(def part-2
  (reduce + (for [row ((comp range count) input)
                  col ((comp range count first) input)
                  :when (= \A (get-in input [row col]))]
              (reduce + (let [pos [row col]
                              se #(-> % south east)
                              sw #(-> % south west)
                              nw #(-> % north west)
                              ne #(-> % north east)
                              directions [se sw nw ne]
                              starts [(nw pos) (ne pos) (se pos) (sw pos)]
                              diagonals (map #(hash-map :pos %1 :dir %2) starts directions)
                              crosses (map vector diagonals (->> diagonals cycle rest (take 4)))]
                          (for [[o t] crosses
                                :let [o' (read-word input (:pos o) (:dir o) 3)
                                      t' (read-word input (:pos t) (:dir t) 3)]
                                :when (= o' t' "MAS")]
                            1))))))

(t/is (= 1974 part-2))