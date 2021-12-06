(ns theianjones.aoc-2021.d05.answer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def rawexample (str/split (slurp (io/resource "day5example.txt")) #"\n"))
(def rawinput (str/split (slurp (io/resource "day5.txt")) #"\n"))

(defn parse-input [input]
  (->> input
       (map #(str/split % #"\-\>"))
       flatten
       (mapcat #(str/split % #","))
       (map #(Long/parseLong (str/trim %)))
       (partition 4)))

(def parsed-example (parse-input rawexample))
(def parsed-input (parse-input rawinput))

(defn initial-board [input]
  (let [maximum (inc (apply max (flatten input)))]
    (into [] (take maximum (repeat (into [] (take maximum (repeat 0))))))))

(def example-board (initial-board parsed-example))
(def input-board (initial-board parsed-input))
;; => ((0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0))
;; draw function that takes a board, and draws the coordinates on the board
;; drawing means incrementing the count from x1,y1 to x2,y2

(defn update-board-row [board [x y]]
  (let [row (nth board y)]
    (update board y (fn [_] (update row x inc)))))

(defn dec-delta [d]
  (if (zero? d) 0 (dec d)))

(defn inc-point [delta point]
  (if (zero? delta) point (inc point)))

(defn line-coords? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn rangex [start end]
  (if (<= start end)
    (range start (inc end))
    (range start (dec end) -1)))

(defn straight-line-points [[x1 y1 x2 y2]]
  (for [x (rangex x1 x2)
        y (rangex y1 y2)]
    [x y]))

(defn diagonal-line-points [[x1 y1 x2 y2]]
  (map vector (rangex x1 x2) (rangex y1 y2)))

(defn line-points [line] 
  (if (line-coords? line)
    (straight-line-points line)
    (diagonal-line-points line)))

(defn draw-line [board coords]
    (let [coords-to-draw (line-points coords)]
    (reduce update-board-row board coords-to-draw)))

(->> (mapcat line-points parsed-input)
(draw-line example-board))

(line-points '(0 9 9 0))
(some #(= (count (set %)) 1) (apply map vector (partition 2 '(9 9 3 9))))

(->> (mapcat line-points parsed-example)
     (frequencies)
     (vals)
     (remove #(== % 1))
     (count))

(defn line
  [[[x1 y1] [x2 y2]] diagonal?]
  (cond (= x1 x2)
        (mapv vector (repeat x1) (rangex y1 y2))
        (= y1 y2)
        (mapv vector (rangex x1 x2) (repeat y1))
        :else (when diagonal?
                (mapv vector (rangex x1 x2) (rangex y1 y2)))))