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

(defn move-submarine [state [direction value]]
  (case direction
    :forward (update state :distance + value)
    :up (update state :depth  - value)
    :down (update state :depth + value)))

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

(defn move-submarin-2 [state [direction value]]
  (case direction
    ;; changed this to use a thread
    ;; dont need a let anymore
    :forward (-> state
                 (update :distance + value)
                 (update :depth + (* value (:aim state)))) ;; dont need an if here, if the aim is 0 then the depth wont change
    :up (update state :aim - value)
    :down (update state :aim + value)))

;; non threaded
(calculate-answer (reduce move-submarin-2 {:depth 0 :distance 0 :aim 0} input))

(def initial-positions
  {:depth 0 :distance 0 :aim 0})

;; threaded!
(->> input
     (reduce move-submarin-2 initial-positions)
     calculate-answer)
