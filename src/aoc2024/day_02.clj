(ns aoc2024.day-02
  (:require [aoc2024.core :as u]))

(defn get-input []
  (->> "resources/day02.input"
       (slurp)
       (u/split-on-lines)
       (mapv #(->> % (u/split-on-spaces) (mapv u/parse-int)))))

(defn remove-idx [coll idx]
  (into (subvec coll 0 idx) (subvec coll (inc idx))))

(defn safe-record? [rec]
  (loop [[[a b] & pairs] ((partition 2 1 rec) rec)
         last-diff nil]
    (let [diff (- b a)
          same-dir? (or (nil? last-diff) (pos? (* last-diff diff)))
          not-zero? (not (zero? diff))
          in-range? (<= (abs diff) 3)
          is-safe? (and same-dir? not-zero? in-range?)]
      (cond
        (not is-safe?) false
        (nil? pairs) true
        :else (recur pairs diff)))))

(defn safe-with-removal? [rec]
  (loop [idx 0]
    (let [with-removal (remove-idx rec idx)
          next-idx (inc idx)]
      (cond
        (safe-record? with-removal) true
        (>= next-idx (count rec)) false
        :else (recur next-idx)))))

; Answer should be 269
(defn part-1 []
  (->> (get-input)
       (map safe-record?)
       (filter true?)
       (count)))

; Answer should be 337
(defn part-2 []
  (->> (get-input)
       (map safe-with-removal?)
       (filter true?)
       (count)))
