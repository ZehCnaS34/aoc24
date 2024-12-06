(ns day-5
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as t])
  (:use [prelude]))

(def input
  (binding [*test* false]
    (as-> (slurp-problem 5 1) $
          (str/join "\n" $)
          (str/split $ #"\n\n")
          {:ordering (->> (str/split (first $) #"\n")
                          (map #(map parse-long (str/split % #"\|")))
                          (reduce (fn [ordering [before after]]
                                    (-> ordering
                                        (update-in [:before after] (fnil conj #{}) before)
                                        (update-in [:after before] (fnil conj #{}) after)))
                                  {:before {}
                                   :after  {}}))
           :updates  (->> (str/split (second $) #"\n")
                          (map #(mapv parse-long (str/split % #","))))})))

(defn valid-position?
  [update i]
  (and
    (set/subset?
      (into #{} (drop (inc i) update))
      (-> input :ordering :after (get (nth update i))))
    (set/subset?
      (into #{} (take i update))
      (-> input :ordering :before (get (nth update i))))))

(defn valid-update?
  [update]
  (let [size (count update)]
    (and
      (every? (partial valid-position? update) (range size))
      (nth update (int (/ size 2))))))

(def part-1
  (->> (:updates input)
       (map valid-update?)
       (remove boolean?)
       (reduce +)))

(t/is (= 4462 part-1))

(defn topological-sort
  [before nodes]
  (let [cleanup-set #(set/difference % (set/difference % (into #{} nodes)))
        before (->> (select-keys before nodes)
                    (map #(vector (key %) (cleanup-set (val %))))
                    (into {}))
        output (atom [])
        temp? (atom #{})
        perm? (atom #{})]
    (letfn [(visit [n]
              (cond
                (@perm? n) nil
                (@temp? n) nil
                :else
                (do
                  (swap! temp? conj n)
                  (doseq [m (before n)]
                    (visit m))
                  (swap! perm? conj n)
                  (swap! output conj n))))]
      (doseq [node nodes
              :when (not (@perm? node))]
        (visit node))
      @output)))

(def part-2-xf
  (let [{:keys [before]} (:ordering input)]
    (comp
      (remove valid-update?)
      (map (partial topological-sort before))
      (map valid-update?))))

(def part-2
  (transduce part-2-xf + 0 (:updates input)))

(t/is (= 6767 part-2))

