(ns theianjones.aoc-2021.d07
  (:require [clojure.string :as str]
            [theianjones.utils :as u]
            [clojure.java.io :as io]))

(def example (map #(Long/parseLong %) (str/split "16,1,2,0,4,2,7,1,2,14" #",")))
(def input (map #(Long/parseLong (str/trim %)) (str/split (slurp (io/resource "day7.txt")) #",")))

(defn fuel [a b] (reduce + (range (inc (Math/abs (- a b))))))

(fuel 16 5)
(fuel 1 5)
(fuel 2 5)
(fuel 0 5)
(take (apply max example))

(defn calculate-costs [acc n]
  (conj acc [n (reduce + (map #(fuel n %) example))]))

(->> (range (apply min example) (apply max example))
     (reduce calculate-costs {})
     (sort-by val)
     (vals)
     first)
