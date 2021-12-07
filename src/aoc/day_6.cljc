(ns aoc.day-6
  (:require [aoc.shared :refer [read-all]]))

(def input-data
  (read-all "aoc/day_6"))

(defn step [{zero 0 :as population}]
  (-> population
      (dissoc 0)
      (update-keys dec)
      (assoc 8 zero)
      (update 6 (fnil + 0 0) zero)))

(defn solve [days]
  (->> input-data
       (frequencies)
       (iterate step)
       (drop days)
       (first)
       (vals)
       (reduce (fnil + 0))))

(defn solve-part-1 []
  (solve 80))

(defn solve-part-2 []
  (solve 256))

(comment

  (solve-part-1) ;; => 350917

  ;; 0.27 msecs
  (time
   (dotimes [_ 100]
     (solve-part-1)))

  (solve-part-2) ;; => 1592918715629

  ;; 0.65 msec
  (time
   (dotimes [_ 100]
     (solve-part-2)))

  )
