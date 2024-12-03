(ns aoc2024.day03
    (:use aoc2024.driver))

(def MUL-PATTERN #"mul\((\d{1,3}),(\d{1,3})\)")

(defsolution day03 [input]
  (let [mul-matches (re-seq MUL-PATTERN input)
        factors (map (fn [m]
                       [(Integer/valueOf (nth m 1))
                        (Integer/valueOf (nth m 2))])
                     mul-matches)]
    [(reduce + (map (fn [[a b]] (* a b)) factors))
     0]))
