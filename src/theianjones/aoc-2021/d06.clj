(ns theianjones.aoc-2021.d06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def rawexample (slurp (io/resource "day6example.txt")))
(def rawinput (slurp (io/resource "day6.txt")))

(def example (mapv #(Long/parseLong %) (str/split rawexample #",")))
(def input (mapv #(Long/parseLong %) (str/split rawinput #",")))

(frequencies example)
;; => {3 2, 4 1, 1 1, 2 1}

(defn n-times [n f]
  (apply comp (repeat n f)))

(prn (count ((n-times 256 day) (frequencies example))))

(defn day [fishies]
  (reduce (fn [new-fishies [fish-days total-fish]]
            (if (zero? fish-days)
              (-> new-fishies
                  (update 6 (fnil + 0) total-fish)
                  (update 8 (fnil + 0) total-fish)
                  (update 0 (fnil - 0) total-fish))
              (-> new-fishies
                  (update fish-days (fnil - 0) total-fish)
                  (update (dec fish-days) (fnil + 0) total-fish)))) fishies fishies))

(prn (apply + (vals (nth (iterate day (frequencies input)) 256))))

;; other solutions
;;
(defn parse [input]
  (as-> input _
    (re-seq #"\d+" _)
    (map #(Integer/parseInt %) _)
    (frequencies _)
    (mapv #(get _ % 0) (range 9))))

(parse rawexample)

(defn step [fishes]
  (conj (update (subvec fishes 1) 6 + (first fishes))
        (first fishes)))

(conj (update (subvec (parse "1,1,1,1,1,1,0,0,0,0,0,2,3,4,5,6,7") 1) 6 + (first (parse rawexample))) (first (parse "1,1,1,1,1,1,0,0,0,0,0,2,3,4,5,6,7")))

(nth (iterate step (parse rawexample)) 80)
