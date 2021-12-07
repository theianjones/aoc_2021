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

(conj (update (subvec (parse "0,1,2,4,4,5,5,5") 1) 6 + (first (parse rawexample))) (first (parse "1,2,4,4,5,5,5,0")))

(def fishes-0 (parse rawexample))
;; => [0 1 1 2 1 0 0 0 0]
(subvec fishes-0 1)
;; => [1 1 2 1 0 0 0 0]
(update (subvec fishes-0 1) 6 + (first fishes-0))
;; => [1 1 2 1 0 0 0 0]
(conj (update (subvec fishes-0 1) 6 + (first fishes-0))
      (first fishes-0))
;; => [1 1 2 1 0 0 0 0 0]
(def fishes-2 [1 2 1 0 0 0 1 0 1])
(subvec fishes-2 1)
;; => [2 1 0 0 0 1 0 1]
(update (subvec fishes-2 1) 6 + (first fishes-2))
;; => [2 1 0 0 0 1 1 1]
(conj (update (subvec fishes-2 1) 6 + (first fishes-2))
      (first fishes-2))
;; => [2 1 0 0 0 1 1 1 1]
;
(def fishes-3 [2 1 0 0 0 1 1 1 1])
(subvec fishes-3 1)
;; => [1 0 0 0 1 1 1 1]
(update (subvec fishes-3 1) 6 + (first fishes-3))
;; => [1 0 0 0 1 1 3 1]
(conj (update (subvec fishes-3 1) 6 + (first fishes-3))
      (first fishes-3))
;; => [1 0 0 0 1 1 3 1 2]
;;
(nth (iterate step (parse rawexample)) 9)
