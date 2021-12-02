(ns aoc.shared
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (java.io PushbackReader)))

(defn ^PushbackReader reader [resource-file]
  (PushbackReader. (io/reader (io/resource resource-file))))

(defn read-all [source]
  (let [rdr (if (instance? PushbackReader source)
              source
              (reader source))]
    (loop [acc []]
      (let [item (edn/read {:eof ::eof} rdr)]
        (if (= item ::eof)
          acc
          (recur (conj acc item)))))))

(comment

  (-> "aoc/day_1.edn"
      (reader)
      (read-all)
      (count))


  )
