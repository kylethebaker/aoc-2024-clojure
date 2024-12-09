(ns aoc2024.day-05
  (:require [aoc2024.core :as u]))

(defn parse-rules [s]
  (->> s
       (u/split-on-lines)
       (map #(->> % (u/split-on #"\|") (map u/parse-int)))
       (reduce
         (fn [m [lhs rhs]]
           (update m lhs #(conj (or % #{}) rhs)))
         {})))

(defn parse-updates [s]
  (->> s
       (u/split-on-lines)
       (map #(->> % (u/split-on #",") (map u/parse-int)))))

(defn get-input []
  (let [input (slurp "resources/day05.sample")
        [rules, updates] (u/split-on #"\n\n" input)]
    [(parse-rules rules) (parse-updates updates)]))

(defn get-middle-val [coll]
  (as-> coll % (count %) (dec %) (/ % 2) (nth coll %)))

(defn all-in-set? [s els]
  (every? #(contains? s %) els))

(defn get-not-in-set [s els]
  (remove #(contains? s %) els))

(defn in-order? [rules [p & ps]]
  (cond
    (empty? ps)
      true
    (all-in-set? (get rules p) ps)
      (recur rules ps)
    :else
      false))

(defn put-in-order [rules ps]
  nil
  )

(get-not-in-set #{13 61 29 47 53} '(97 47 61 53))
(get-not-in-set #{75 13 61 29 47 53} '(13 75 29 47))

; Answer should be 4569
(defn part-1 []
  (let [[rules updates] (get-input)]
    (->> updates
         (filter #(in-order? rules %))
         (map get-middle-val)
         (apply +))))

(defn part-2 []
  (let [[rules updates] (get-input)]
    (->> updates
         (remove #(in-order? rules %))
         (map #(put-in-order rules %))
         (map get-middle-val)
         (apply +))))

(part-1)
(get-input)
