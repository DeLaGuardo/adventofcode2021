(ns aoc.day-4
  (:require [aoc.shared :refer [read-all]]))

(defn make-board [xs]
  (into {}
        (for [i (range 5)
              j (range 5)
              :let [x (nth xs (+ j (* 5 i)))]]
          [x {:row (into #{} (for [j (range 5)]
                               (nth xs (+ j (* 5 i)))))
              :col (into #{} (for [i (range 5)]
                               (nth xs (+ j (* 5 i)))))}])))

(defn data []
  (let [[x & xs] (read-all "aoc/day_4")]
    {:input x
     :boards (into []
                   (comp (partition-all 25)
                         (map make-board))
                   xs)}))

(defn check-number [n board]
  (if (contains? board n)
    (let [board (-> board
                    (assoc-in [n :checked] true)
                    (update-in [n :row] disj n)
                    (update-in [n :col] disj n))
          {:keys [row col]} (get board n)]
      (if (or (not (seq row))
              (not (seq col)))
        (assoc board :score (* n (transduce (keep (fn [[n {:keys [checked]}]]
                                                    (when-not checked n)))
                                            + board)))
        (as-> board $
          (reduce
           (fn [board n1]
             (update-in board [n1 :row] disj n))
           $
           row)
          (reduce
           (fn [board n1]
             (update-in board [n1 :col] disj n))
           $
           col))))
    board))

(defn solver [[n & inputs] boards]
  (lazy-seq
   (when n
     (let [{winners true
            boards false} (group-by #(contains? % :score) (map #(check-number n %) boards))]
       (if (seq winners)
         (cons (-> winners (first) :score) (solver inputs boards))
         (solver inputs boards))))))

(defn solve-part-1 []
  (let [{:keys [input boards]} (data)]
    (first (solver input boards))))

(defn solve-part-2 []
  (let [{:keys [input boards]} (data)]
    (last (solver input boards))))

(comment

  (solve-part-1) ;; => 8580

  (solve-part-2) ;; => 9576

  ;; 21 msecs
  (time
   (dotimes [_ 100]
     (solve-part-1)))

  ;; 26 msecs
  (time
   (dotimes [_ 100]
     (solve-part-2)))

  )
