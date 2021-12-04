(ns theianjones.aoc-2021.d04.answer
  (:require [theianjones.utils :as u]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def example (str/split (slurp (io/resource "day4example.txt")) #"\n\n"))
(def i (str/split (slurp (io/resource "day4.txt")) #"\n\n"))

(defn str-to-ints
  [string]
  (map #(Long/parseLong %) (remove empty?
                                   (str/split string #" "))))

(defn parse-input [input]
  (let [numbers (first input)
        boards (map #(str/split % #"\n") (rest input))]
    [(map #(Long/parseLong %) (str/split numbers #",")) (map (fn [board] (map #(str-to-ints %) board)) boards)]))

(def example-boards (parse-input example))
(def input-boards (parse-input i))

(defn winner? [input row]
  (= (count (set/intersection (set input) (set row))) (count row)))

(winner? '(1 2 3 4 5) '(2 3 4 5 1))

(defn get-column [board n]
  (map #(nth % n) board))

(defn column-winner? [board input n]
  (winner? input (get-column board n)))

(defn row-winner? [board input n]
  (winner? input (nth board n)))

(defn board-winner? [board input]
  (reduce (fn [found-winner index]
            (if found-winner
              (reduced found-winner)
              (or
               (row-winner? board input index)
               (column-winner? board input index))))
          false
          (range (count (first board)))))

(defn calculate-winner [board input]
  (* (last input)
     (apply + (remove (set input) (flatten board)))))

(defn calculate [total-input boards]
  (loop [n 5]
    (let [current-input (take n total-input)
          winning-board (first
                         (filter
                          #(board-winner? % current-input)
                          boards))
          _ (prn current-input)]
      (if (or winning-board (= n (count total-input)))
        (calculate-winner winning-board current-input)
        (recur (inc n))))))

(defn get-last-winning-board [boards current-input]
  (let [winning-boards (filter
                        #(board-winner? % current-input) boards)]
    (when (= (count winning-boards) (dec (count boards)))
      (first (remove #(board-winner? % current-input) boards)))))

(get-last-winning-board (second example-boards) '(7 4 9 5 11 17 23 2 0 14 21 24 10 16))

(defn calculate-last-winner [total-input boards]
  (loop [n 5
         last-board-to-win '()]
    (let [current-input (take n total-input)]
      (if (or
           (and last-board-to-win (board-winner? last-board-to-win current-input))
           (= n (count total-input)))
        (calculate-winner last-board-to-win current-input)
        (recur (inc n) (get-last-winning-board boards current-input))))))

(calculate-last-winner (first input-boards) (second input-boards))

(calculate (first input-boards) (second input-boards))

(def first-board (-> example-boards
                     second
                     last))

(board-winner? first-board '(7 4 9 5 11 17 23 2 0 14 21 21 24))
(first (filter #(board-winner? % '(7 4 9 5 11 17 23 2 0 14 21 24)) (second example-boards)))

(apply + (remove (set '(7 4 9 5 11 17 23 2 0 14 21 24)) (flatten first-board)))

(range (count (first first-board)))

(calculate-winner first-board '(7 4 9 5 11 17 23 2 0 14 21 24))

;; fave community solutions
;; https://github.com/tschady/advent-of-code/blob/main/src/aoc/2021/d04.clj
;; https://github.com/nbardiuk/adventofcode/blob/master/2021/src/day04.clj
