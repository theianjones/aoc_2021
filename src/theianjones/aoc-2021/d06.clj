(ns theianjones.aoc-2021.d06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def rawexample (slurp (io/resource "day6example.txt")))

(def example (mapv #(Long/parseLong %) (str/split rawexample #",")))

(defn day [fishies]
  (reduce-kv (fn [new-fishies index fish]
               (prn  i)
               (if (zero? fish)
                 (conj (update new-fishies index (fn [_] 6)) 8)
                 (update new-fishies index dec))) fishies fishies))

(defn n-times [n f]
  (apply comp (repeat n f)))

((n-times 80 day) example)