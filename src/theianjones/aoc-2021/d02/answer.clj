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

(let [result (reduce move-submarine {:depth 0 :distance 0} input)
      answer1 (* (:depth result) (:distance result))]
  answer1)
