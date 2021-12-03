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

(defn read-all-lines
  ([source] (read-all-lines identity source))
  ([f source]
   (with-open [rdr (io/reader (io/resource source))]
     (mapv f (line-seq rdr)))))

