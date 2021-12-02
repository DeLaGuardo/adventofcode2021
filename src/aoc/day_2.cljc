(ns aoc.day-2
  (:require [aoc.shared :refer [read-all]]))

(defn solver []
  (reduce
   (fn [[x y z] [op value]]
     (case op
       forward [(+ x value) (+ y (* z value)) z]
       down    [x           y                 (+ z value)]
       up      [x           y                 (- z value)]))
   [0 0 0]
   (partition 2 (read-all "aoc/day_2.edn"))))

(comment

  (let [[x y z] (solver)]
    [(* x z) (* x y)]) ;; => [2027977 1903644897]

  )
