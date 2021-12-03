(ns theianjones.aoc-2021.d03.answer
  (:require
   [theianjones.utils :as u]
   [clojure.string :as str]))

(defn to-binary-string
  [& col]
  (apply str col))

(def example (map (partial apply to-binary-string) (u/read-resource "day3example.txt")))

(defn to-int-seq [s]
  (map #(Long/parseLong %) (str/split s #"")))

(map #(if (> % (/ (count example) 2)) 1 0)
     (apply map + (map to-int-seq example)))

(defn count-bits [[& col]])

(defn sort-bytes [col]
  (map #(sort-by (fn [char] (if (= char \0) 0 1)) %) col))

(def part-bytes (partial partition-by #(= \0 %)))

(map part-bytes (sort example))

(defn parse-binary
  [s]
  (try
    (Long/parseLong s 2)
    (catch Exception _)))
