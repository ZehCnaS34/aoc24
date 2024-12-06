(ns day-6
  (:refer-clojure :exclude [get get-in])
  (:require [clojure.core :as c]
            [clojure.set :as set]
            [clojure.test :as t])
  (:use [prelude]))

(def ^:dynamic *board*
  (binding [*test* false]
    (->>
      (load-problem)
      (parse-grid))))

(defn bounds
  [board]
  {:rows (count board)
   :cols (count (first board))})

(defn collide?
  [board pos]
  (= \# (c/get-in board pos)))

(defn in-bounds?
  [board [row col]]
  (let [{:keys [rows cols]} (bounds board)]
    (and (<= 0 row (dec rows))
         (<= 0 col (dec cols)))))

(def rotation
  (as-> (cycle "^>V<") $
        (map vector $ (rest $))
        (take 4 $)
        (into {} $)))

(def guard? (into #{} (keys rotation)))

(def guard->direction
  {\^ #(update % 0 dec)
   \> #(update % 1 inc)
   \V #(update % 0 inc)
   \< #(update % 1 dec)})

(defn find-guard
  [board]
  (let [{:keys [rows cols]} (bounds board)]
    (->> (for [row (range rows)
               col (range cols)
               :when (guard? (c/get-in board [row col]))]
           [row col])
         (first))))

(defn project
  ([board]
   (let [src (find-guard board)
         guard (c/get-in board src)
         direction (guard->direction guard)
         path (take-while
                (every-pred
                  (complement (partial collide? board))
                  (partial in-bounds? board))
                (project src direction))
         dest (last path)
         target (direction dest)]
     {:src       src
      :dest      dest
      :target    target
      :guard     guard
      :path      path
      :positions (into #{} path)}))
  ([pos direction]
   (cons pos (lazy-seq (project (direction pos) direction)))))

(defn move-guard
  [board {:keys [src dest guard]}]
  (-> board
      (c/assoc-in src \.)
      (c/assoc-in dest (rotation guard))))

(defn walk
  ([]
   (walk *board*))
  ([board]
   (walk board #{}))
  ([board positions]
   (let [projection (project board)]
     (if-not (in-bounds? board (:target projection))
       (count (set/union positions (:positions projection)))
       (recur (move-guard board projection)
              (set/union positions (:positions projection)))))))

(def part-1 (walk))

(t/is (= 5461 part-1))