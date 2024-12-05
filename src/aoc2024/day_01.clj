(ns aoc2024.day-01
  (:require [aoc2024.core :as u]))

(defn parse-input [s]
  (u/parse-re-seq
    #"(\d+)\s+(\d+)\n"
    [[:left u/parse-int] [:right u/parse-int]]
    s))

(defn get-lists []
  (->> "./resources/day01.input"
       (slurp)
       (parse-input)
       (map (juxt :left :right))
       (u/transpose)))

; Answer should be 2285373
(defn part-1 []
  (->> (get-lists)
       (map sort)
       (apply u/zip)
       (map #(abs (- (first %) (last %))))
       (apply +)))

; Answer should be 21142653
(defn part-2 []
  (let [[ls rs] (get-lists)
        freqs (frequencies rs)
        ls-scored (map #(* % (get freqs % 0)) ls)
        summed (apply + ls-scored)]
    summed))
