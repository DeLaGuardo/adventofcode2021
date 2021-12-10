(ns aoc.day-9
  (:require [aoc.shared :refer [read-all-lines]]))

(def input-data
  (read-all-lines
   (fn [line]
     (mapv (into {} (map (fn [n] [(first (str n)) n]) (range 10))) line))
   "aoc/day_9"))

(defn get-height [i j]
  (get (get input-data i) j 10))

(defn lowest-points []
  (for [i (range (count input-data))
        j (range (count (first input-data)))
        :let [t (get-height (inc i) j)
              l (get-height i (dec j))
              r (get-height i (inc j))
              b (get-height (dec i) j)
              x (get-height i j)]
        :when (every? #(> % x) [t l r b])]
    [x i j]))

(defn solve-part-1 []
  (transduce
   (comp
    (map first)
    (map inc))
   +
   (lowest-points)))

(defn basin [[_ i j]]
  (loop [res #{} [[i j :as point] & to-check :as coords] [[i j]]]
    (if (seq coords)
      (if (and (< (get-height i j) 9)
               (not (contains? res point)))
        (recur (conj res point) (conj to-check
                                      (vector (inc i) j)
                                      (vector i (dec j))
                                      (vector i (inc j))
                                      (vector (dec i) j)))
        (recur res to-check))
      (count res))))

(defn solve-part-2 []
  (->> (lowest-points)
       (map basin)
       (sort >)
       (take 3)
       (reduce *)))

(comment

  (solve-part-1) ;; => 548

  ;; 3.5 msecs
  (time
   (dotimes [_ 1000]
     (solve-part-1)))

  (solve-part-2) ;; => 786048

  ;; 15 msecs
  (time
   (dotimes [_ 10]
     (solve-part-2)))

  )
