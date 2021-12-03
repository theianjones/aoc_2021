(ns theianjones.aoc-2021.d03.answer
  (:require
   [theianjones.utils :as u]
   [clojure.string :as str]))

(defn to-binary-string
  [& col]
  (apply str col))

(def example (map (partial apply to-binary-string) (u/read-resource "day3example.txt")))
(def input (map (partial apply to-binary-string) (u/read-resource "day3.txt")))

(defn to-int-seq [s]
  (map #(Long/parseLong %) (str/split s #"")))

(defn get-rates [col]
  (let [gamma (map #(if (>= % (/ (count col) 2)) 1 0)
                   (apply map + (map to-int-seq col)))
        epsilon (map #(if (= 0 %) 1 0) gamma)]
    [(apply str gamma) (apply str epsilon)]))

(defn parse-binary
  [s]
  (try
    (Long/parseLong s 2)
    (catch Exception _)))

(apply * (map parse-binary (get-rates example)))

(defn get-ratings [col n pred]
  (let [threshold (/ (count col) 2)
        positive-bit-count (nth (apply map + col) n)
        rating-filter (if (pred positive-bit-count threshold) 1 0)]
    (filter #(= (nth % n) rating-filter) col)))

(defn find-rating [pred col]
  (loop [n 1
         result (get-ratings (map to-int-seq col) 0 pred)]
    (if (or (= (count col) n) (= (count result) 1))
      (apply str (flatten result))
      (recur (inc n) (get-ratings result n pred)))))

(def oxygen-rating (partial find-rating >=))
(def c02-rating (partial find-rating <))

(->> input
     ((juxt oxygen-rating c02-rating))
     (map parse-binary)
     (apply *))
