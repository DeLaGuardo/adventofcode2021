(ns aoc.day-10
  (:require [aoc.shared :refer [read-all-lines]]))

(defn op [ch]
  (case ch
    (\{ \[ \( \<) (fnil inc 0)
    (\} \] \) \>) (fnil dec 0)))

(defn pair? [x]
  ((some-fn #(= (seq "()") %)
            #(= (seq "[]") %)
            #(= (seq "{}") %)
            #(= (seq "<>") %))
   x))

(defn open? [ch]
  (contains? (set "({[<") ch))

(defn count-
  ([line] (count- () line))
  ([[open & rest-open :as state] [ch & line]]
   (cond
     (open? ch)
     (recur (cons ch state) line)

     (pair? [open ch])
     (recur rest-open line)

     :else [ch (map {\{ \} \[ \] \( \) \< \>} state)])))

(def points
  {\> 25137
   \} 1197
   \] 57
   \) 3})

(defn solve-part-1 []
  (transduce
   (comp (map first)
         (keep identity)
         (map points))
   +
   (read-all-lines count- "aoc/day_10")))

(def points-2
  {\> 4
   \} 3
   \] 2
   \) 1})

(defn compute-score [xs]
  (transduce
   (map points-2)
   (fn
     ([acc] acc)
     ([acc x]
      (+ (* acc 5) x)))
   0
   xs))

(defn solve-part-2 []
  (let [xs (sort (sequence
                  (comp (filter (complement first))
                        (map second)
                        (map compute-score))
                  (read-all-lines count- "aoc/day_10")))
        i (/ (dec (count xs)) 2)]
    (nth xs i)))

(comment

  (solve-part-1) ;; => 318099

  ;; 12 msecs
  (time
   (dotimes [_ 1000]
     (solve-part-1)))

  (solve-part-2) ;; => 2389738699

  ;; 12 msecs
  (time
   (dotimes [_ 1000]
     (solve-part-2)))

  )
