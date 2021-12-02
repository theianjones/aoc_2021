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

(defn set-position [key value]
  (swap! position assoc key (+ value (get @position key))))

(set-position :depth 1)

(- 1)

(defn move-submarine [[key value]]
  (case key
    :forward (set-position :distance value)
    :up (set-position :depth (- value))
    :down (set-position :depth value)))

(run! move-submarine example)

(run! move-submarine input)

(* (:depth @position) (:distance @position))
