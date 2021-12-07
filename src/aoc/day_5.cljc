(ns aoc.day-5
  (:require [aoc.shared :refer [read-all]]))

(def input-data
  (sequence
   (comp (partition-all 5)
         (map (fn [[x1 y1 _ x2 y2]]
                [[x1 y1] [x2 y2]])))
   (read-all "aoc/day_5")))

(defn h-or-v? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn diagonal? [[[^long x1 ^long y1] [^long x2 ^long y2]]]
  (= 1.0 (Math/abs (float (/ (- y2 y1) (- x2 x1))))))

(defn line->points [line]
  (let [[[x1 y1] [x2 y2]] (sort line)]
    (if (= x1 x2)
      (map vector (repeat x1) (range y1 (inc y2)))
      (for [x (range x1 (inc x2))
            :let [y (+ y2 (/ (* (- y2 y1)
                                (- x x2))
                             (- x2 x1)))]]
        [x y]))))

(defn solver [f]
  (->> (sequence
        (comp (filter f)
              (mapcat line->points))
        input-data)
       (frequencies)
       (vals)
       (filter #(> % 1))
       (count)))

(defn solve-part-1 []
  (solver h-or-v?))

(defn solve-part-2 []
  (solver (some-fn h-or-v? diagonal?)))

(comment

  (solve-part-1) ;; => 6007

  ;; 106 msecs
  (time
   (dotimes [_ 100]
     (solve-part-1)))

  (solve-part-2) ;; => 19349

  ;; 193 msecs
  (time
   (dotimes [_ 100]
     (solve-part-2)))

  )
