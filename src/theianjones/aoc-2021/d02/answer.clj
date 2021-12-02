(ns theianjones.aoc-2021.d02.answer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-instruction [instruction]
  (let [[d u] (str/split instruction #" ")
        direction (keyword d)
        unit (Long/parseLong u)]
    [direction unit]))

(def input
  (->>
   (io/resource "day2.txt")
   io/reader
   line-seq
   (mapv #(parse-instruction %))))

(def example [[:forward 5]
              [:down 5]
              [:forward 8]
              [:up 3]
              [:down 8]
              [:forward 2]])

(def position (atom {:depth 0 :distance 0}))

(defn set-position [state key value]
  (assoc state key (+ value (get state key))))

(set-position {:depth 0 :distance 0} :depth 1)

(- 1)

(defn move-submarine [state [key value]]
  (case key
    :forward (set-position state :distance value)
    :up (set-position state :depth (- value))
    :down (set-position state :depth value)))

(run! move-submarine example)

;; Ive been wanting to use Juxt lol
;; found this in Borkdudes answer
;; https://gist.github.com/borkdude/b5cf0e9d2d8ab7c678d88e27a3357b33#file-aoc21_02-clj
(defn calculate-answer [state]
  (->> state
       ((juxt :depth :distance))
       (apply *)))

(let [result (reduce move-submarine {:depth 0 :distance 0} input)
      answer1 (calculate-answer result)]
  answer1)

(defn move-submarin-2 [state [key value]]
  (case key
    :forward (let [newForwardState (set-position state :distance value)
                   currentAim (:aim state)]
               (if (> currentAim 0)
                 (set-position newForwardState :depth (* value currentAim))
                 newForwardState))
    :up (set-position state :aim (- value))
    :down (set-position state :aim value)))

;; non threaded
(calculate-answer (reduce move-submarin-2 {:depth 0 :distance 0 :aim 0} input))

(def initial-positions
  {:depth 0 :distance 0 :aim 0})

;; threaded!
(->> input
     (reduce move-submarin-2 initial-positions)
     calculate-answer)
