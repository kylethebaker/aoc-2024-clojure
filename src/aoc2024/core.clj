(ns aoc2024.core
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn log
  "Pretty prints a value and returns it, to make adding logs to chains easier"
  [v] (pp/pprint v) v)

(defmacro to-map
  "Creates a map using the symbol names as keys"
  [& syms]
  (zipmap (map keyword syms) syms))

(defn zip
  "Zips all of the lists together"
  [& lists] (apply map vector lists))

(defn parse-int
  "Parses a string into an integer"
  [s] (Integer/parseInt s))

(defn split-on-spaces
  "Splits a string on whitespace"
  [s] (str/split s #"\s+"))

(defn split-on-lines
  "Splits a string on newlines"
  [s] (str/split-lines s))

(defn transpose
  "
  Transposes a simple nested collection
  [[a1 a2 a3] [b1 b2 b3]] becomes [[a1 b1] [a2 b2] [a3 b3]]
  "
  [nested-col]
  (apply mapv vector nested-col))

(defn- parse-re-seq-item [defs matches]
  (let [[names parsers] (apply zip defs)
        joined (zip parsers names (rest matches))
        fields (map (fn [[parse-fn key-name v]] [key-name (parse-fn v)]) joined)]
    (if (not-empty fields) (into {} fields) nil)))

(defn parse-re-seq
  "
  Parses a string into a list of records using re-seq. You have to use capture
  groups in the regex, and the keys/parsers in the defs list correspond
  positionally to the capture groups
  "
  [re defs s]
  (map #(parse-re-seq-item defs %) (re-seq re s)))

(defn parse-re [re defs s]
  (->> s (re-find re) (parse-re-seq-item defs)))

(defn- parse-res-item [re-defs match]
  (let [re-def (some? #(parse-res

(defn parse-res [re-defs s]
  (let [regexes (map :regex re-defs)
        root-regex (re-pattern (str "(" (str/join "|" regexes) ")"))
        matches (re-seq root-regex s)]
    (map (fn [match]
  ))))

;;(parse-res 
;;  [{:name "muls"
;;   :regex #"(\d+)(\d)+"
;;   :groups [[:lhs parse-int]
;;            [:rhs parse-int]]}
;;   {:name "do" :regex #"do()"}
;;   {:name "dont" :regex #"don't()"}

