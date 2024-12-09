(ns aoc2024.day-03
  (:require [aoc2024.core :as u]
            [clojure.core.match :refer [match]]))

(defn get-input []
  (->> "resources/day03.input"
       (slurp)
       (u/parse-res
         [{:name :mult
           :regex #"mul\((\d{1,3}),(\d{1,3})\)"
           :groups [[:lhs u/parse-int] [:rhs u/parse-int]]}
          {:name :do :regex #"do\(\)"}
          {:name :dont :regex #"don't\(\)"}])))

(defn mult [{a :lhs b :rhs }] (* a b))

; Answer should be 160672468
(defn part-1 []
  (->> (get-input)
       (filter #(= :mult (first %)))
       (map #(mult (second %)))
       (apply +)))

; Answer should be 84893551
(defn part-2 []
  (reduce
    (fn [[enabled sum] ins]
      (match [enabled ins]
        [true [:mult args]] [true (+ sum (mult args))]
        [_ [:dont _]] [false sum]
        [_ [:do _]] [true sum]
        :else [enabled sum]))
    [true 0]
    (get-input)))
