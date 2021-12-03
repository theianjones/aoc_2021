(ns theianjones.utils
  (:require [clojure.java.io :as io]))

(defn read-resource [file-name]
  (->>
   (io/resource file-name)
   io/reader
   line-seq))
