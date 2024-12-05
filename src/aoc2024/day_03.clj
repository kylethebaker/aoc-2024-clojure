(ns aoc2024.day-03
  (:require [aoc2024.core :as u]))

(def rgx-mul #"mul\((\d{1,3}),(\d{1,3})\)")
(def rgx-do #"do\(\)")
(def rgx-dont #"don't\(\)")

(defn parse-muls [s]
  (u/parse-re-seq rgx-mul [[:lhs u/parse-int] [:rhs u/parse-int]] s))

;;(defn parse-muls-with-toggles [s]
;;  (u/parse-re-seq
;;    #"(don't\(\)|mul\((\d{1,3}),(\d{1,3})\))"
;;    [[:lhs u/parse-int] [:rhs u/parse-int]]
;;    s))

(defn parse-muls-with-toggles [s]
  (re-seq (re-pattern (str "(" rgx-do "|" rgx-dont "|" rgx-mul ")")) s))

(parse-muls-with-toggles
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn get-input []
  (->> "resources/day03.input"
       (slurp)
       (u/parse-re-seq
         #"mul\((\d{1,3}),(\d{1,3})\)"
         [[:lhs u/parse-int] [:rhs u/parse-int]])))

(defn mult [{a :lhs b :rhs }] (* a b))

; Answer should be 160672468
(defn part-1 []
  (->> (get-input)
       (map mult)
       (apply +)))

(part-1)
