(ns aoc.day-1
  (:require [aoc.shared :refer [read-all]]))

(defn solver [n data]
  (count (for [xs (partition n 1 data)
               :when (pos? (- (last xs) (first xs)))]
           nil)))

(defn solve-part-1 []
  (solver 2 (read-all "aoc/day_1.edn")))

(defn solve-part-2 []
  (solver 4 (read-all "aoc/day_1.edn")))

(comment

  (solve-part-1) ;; => 1400
  (solve-part-2) ;; => 1429

  )
