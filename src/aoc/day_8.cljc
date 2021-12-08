(ns aoc.day-8
  (:require [aoc.shared :refer [read-all-lines]]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input-data
  (read-all-lines (fn [line]
                    (let [[input [_ & output]] (split-with #(not= "|" %) (string/split line (re-pattern " ")))
                          input (map #(into #{} %) input)
                          output (map #(into #{} %) output)]
                      {:input input
                       :output output}))
                  "aoc/day_8"))

(defn solve-part-1 []
  (count
   (sequence
    (comp
     (mapcat :output)
     (filter #(#{2 4 3 7} (count %))))
    input-data)))

(defn deduct-0-6-9 [samples {one 1
                             four 4
                             :as digits}]
  (into digits (for [sample samples
                     :let [x ((juxt (partial set/subset? one)
                                    (partial set/subset? four))
                              sample)]]
                 (case x
                   [true false]  [0 sample]
                   [true true]   [9 sample]
                   [false false] [6 sample]))))

(defn deduct-2-3-5 [samples {six 6
                             nine 9
                             :as digits}]
  (into digits (for [sample samples
                     :let [x ((juxt #(set/subset? % six)
                                    #(set/subset? % nine))
                              sample)]]
                 (case x
                   [false false] [2 sample]
                   [false true]  [3 sample]
                   [true true]   [5 sample]))))

(defn make-number [xs m]
  (reduce + (map (fn [x k]
                   (* k (m x)))
                 xs
                 '(1000 100 10 1))))

(defn deduct [{:keys [input output]}]
  (let [{[one] 2
         [seven] 3
         [four] 4
         samples-0-6-9 6
         samples-2-3-5 5
         [eight] 7} (group-by count input)
        digits {1 one
                4 four
                7 seven
                8 eight}]
    (->> digits
         (deduct-0-6-9 samples-0-6-9)
         (deduct-2-3-5 samples-2-3-5)
         (reduce-kv #(assoc %1 %3 %2) {})
         (make-number output))))

(defn solve-part-2 []
  (transduce (map deduct) + input-data))

(comment

  (solve-part-1) ;; => 476

  ;; 0.13 msecs
  (time
   (dotimes [_ 10000]
     (solve-part-1)))

  (solve-part-2) ;; => 1011823

  ;; 5 msecs
  (time
   (dotimes [_ 100]
     (solve-part-2)))

  )
