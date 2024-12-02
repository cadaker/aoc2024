(ns aoc2024.day01
  (:use aoc2024.driver))

(def INPUT-LINE #"(\d+)\s+(\d+)\n")

(defn iabs [x]
  (if (> x 0) x (- x)))

(defn diff [x1 x2]
  (iabs (- x1 x2)))

(defn inc* [x]
  (inc (or x 0)))

(defn make-counts [xs]
  (reduce (fn [acc x]
            (update acc x inc*))
          {}
          xs))

(defn sum [xs]
  (reduce + 0 xs))

(defsolution day01 [input]
  (let [matches (re-seq INPUT-LINE input)
        list1 (sort (map (fn [match]
                           (Integer/valueOf (nth match 1)))
                         matches))
        list2 (sort (map (fn [match]
                           (Integer/valueOf (nth match 2)))
                         matches))
        diffs (map diff list1 list2)
        counts (make-counts list2)]
    [
      (sum diffs)
      (sum (map (fn [x]
                  (* x (get counts x 0)))
                list1))
      ]))
