(ns aoc2024.day01
  (:use aoc2024.driver))

(def INPUT-LINE #"(\d+)\s+(\d+)\n")

(defn iabs [x]
  (if (> x 0) x (- x)))

(defn diff [x1 x2]
  (iabs (- x1 x2)))

(defsolution day01 [input]
  (let [matches (re-seq INPUT-LINE input)
        pairs (map
               (fn [match]
                 [(Integer/valueOf (nth match 1))
                  (Integer/valueOf (nth match 2))])
               matches)
        list1 (sort (map first pairs))
        list2 (sort (map second pairs))
        diffs (map diff list1 list2)]
    [(reduce + diffs) 0]))
