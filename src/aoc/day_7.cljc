(ns aoc.day-7
  (:require [aoc.shared :refer [read-all]]))

(def input-data
  (read-all "aoc/day_7"))

(defn solver [f]
  (let [x (apply min input-data)
        y (apply max input-data)
        positions (frequencies input-data)]
    (first (sort (map (fn [pos]
                        (reduce-kv
                         (fn [acc loc amount]
                           (+ acc (* amount (f loc pos))))
                         0
                         positions))
                      (range x (inc y)))))))

(defn solve-part-1 []
  (solver #(Math/abs (int (- %1 %2)))))

(defn solve-part-2 []
  (solver (fn [pos dir]
            (let [n (Math/abs (- pos dir))]
              (/ (* n (inc n)) 2)))))

(comment

  (solve-part-1) ;; => 336131

  ;; 52 msecs
  (time
   (dotimes [_ 100]
     (solve-part-1)))

  (solve-part-2) ;; => 92676646

  ;; 177 msecs
  (time
   (dotimes [_ 100]
     (solve-part-2)))

  )
