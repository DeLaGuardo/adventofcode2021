(ns aoc.day-3
  (:require [aoc.shared :refer [read-all-lines]]))

(def input
  (read-all-lines seq "aoc/day_3"))

(defn to-long [xs]
  (Long/parseLong (apply str xs) 2))

(defn solve-part-1 []
  (->> input
       (reduce
        (fn [acc line]
          (map (fn [x y]
                 (update x y (fnil inc 0)))
               acc line))
        (repeat (count (first input)) nil))
       (map (fn [{one \1 zero \0}]
              (if (> one zero)
                {:gamma 1
                 :epsilon 0}
                {:gamma 0
                 :epsilon 1})))
       ((juxt #(map :gamma %) #(map :epsilon %)))
       (map to-long)
       (reduce *)))

(defn rating [criteria input]
  (loop [input input pos 0]
    (if (or (= 1 (count input))
            (= pos (count (first input))))
      (to-long (first input))
      (let [{zero \0 one \1 :or {zero 0 one 0}} (frequencies (map #(nth % pos) input))
            n (if (criteria zero one) \1 \0)]
        (recur (filter #(= n (nth % pos)) input) (inc pos))))))

(defn solve-part-2 []
  (* (rating > input)
     (rating <= input)))

(comment

  (solve-part-1) ;; => 1025636

  (solve-part-2) ;; => 793873

  )
