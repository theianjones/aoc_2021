(ns theianjones.aoc-2021.d01.answer
  (:require
   [clojure.java.io :as io]))

(def example [199
              200
              208
              210
              200
              207
              240
              269
              260
              263])

(def input
  (->>
   (io/resource "day1.txt")
   io/reader
   line-seq
   (mapv #(Long/parseLong %))))
;; scratch pad

(reduce-kv (fn [count index value] (if (< value (nth [1 2 3 4] (inc index) 0))
                                     (inc count)
                                     count)) 0 [1 2 3 4])
(def initial-count 0)

(reduce-kv (fn [count index value]
             (if (< value (nth input (inc index) -1))
               (inc count)
               count))
           initial-count input)

;; the real answer

;; this will take any collection and cound how many times (< (nth col n) (nth col (inc n))) has increased
(defn count-increased [col]
  (reduce-kv (fn [count index value]
               (if (< value (nth col (inc index) -1))
                 (inc count)
                 count))
             initial-count col))

(count-increased (into [] (remove nil? (map-indexed (fn [index n]
                                                      (let [n2 (nth input (inc index) nil)
                                                            n3 (nth input (inc (inc index)) nil)]
                                                        (when (and (some? n2)
                                                                   (some? n3))
                                                          (+ n n2 n3)))) input))))

(defn add-not-nil [& col]
  (when (not (some nil? col)) (reduce + col)))

(add-not-nil 1 2)

;; I realized that you can just build the windows as you go to avoid multiple iterations
;; I havent tested either of the solutions so dont really know which is faster
(defn optimized [col]
  (reduce-kv (fn [count index value]
               (let [value2 (nth col (inc index) nil)
                     value3 (nth col (inc (inc index)) nil)
                     value4 (nth col (inc (inc (inc index))) nil)
                     window1 (add-not-nil value value2 value3)
                     window2 (add-not-nil value2 value3 value4)]
                 (if (and  (not (nil? window1)) (not (nil? window2)))
                   (if (< window1 window2)
                     (inc count)
                     count)
                   count))) 0 col))

(optimized input)

;; looked at community solutions
;; 2 key things i missed
;; apply: being able to apply a function to a collection
;; partition: being able to split up a collection into sub collections
;;

(defn increased-count [col] (count (filter #(apply < %) (partition 2 1 col))))

(increased-count example)

(increased-count input)

(def puzzle2 (increased-count (map #(apply + %) (partition 3 1 input))))
