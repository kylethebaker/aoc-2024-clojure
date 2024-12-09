(ns aoc2024.day-04
  (:require [aoc2024.core :as u]
            [clojure.math.combinatorics :refer [permuted-combinations]]))

(defn get-input []
  (->> "resources/day04.input"
       (slurp)
       (u/split-on-lines)
       (map char-array)
       (vec)))

(defn get-points-for-grid [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn add-point [[y1 x1] [y2 x2]]
  [(+ y1 y2) (+ x1 x2)])

(defn has-cross-mas? [grid pos]
  (and
    (= (get-in grid pos) \A)
    (->> [[-1 -1] [-1 1] [1 1] [1 -1]]
         (map #(get-in grid (add-point pos %)))
         (contains? #{[\M \M \S \S]
                      [\S \M \M \S]
                      [\S \S \M \M]
                      [\M \S \S \M]}))))

(defn has-xmas-in-dir? [grid pos offset]
  (loop [[needed & ls] [\X \M \A \S]
         cur pos]
    (cond
      (not= (get-in grid cur) needed) false
      (empty? ls) true
      :else (recur ls (add-point cur offset)))))

(defn get-xmas-dirs [grid pos]
  (->> (permuted-combinations [-1 -1 0 1 1] 2)
       (filter #(has-xmas-in-dir? grid pos %))))

; Answer should be 2378
(defn part-1 []
  (let [grid (get-input) points (get-points-for-grid grid)]
    (->> points
         (map #(get-xmas-dirs grid %))
         (filter not-empty)
         (apply concat)
         (count))))

; Answer should be 1796
(defn part-2 []
  (let [grid (get-input) points (get-points-for-grid grid)]
    (->> points
         (filter #(has-cross-mas? grid %))
         (count))))
