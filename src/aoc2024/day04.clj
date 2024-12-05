(ns aoc2024.day03
    (:use aoc2024.driver aoc2024.grid))

(defn parse-input [input]
    (gridbuilder-finish
        (reduce (fn [builder ch]
                    (if (= ch \newline)
                        (gridbuilder-eol builder)
                        (gridbuilder-push builder ch)))
                (make-gridbuilder)
                input)))

(def PATTERN "XMAS")

(def DIRECTIONS '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]))

(defn pattern-scans? [grid [row col] [drow dcol] pattern]
    (let [len1 (dec (count pattern))
          points (iterate (fn [[r c]] [(+ r drow) (+ c dcol)]) [row col])
          sampled-points (map #(grid-getp grid %) points)]
        (if (and (grid-contains? grid row col)
                 (grid-contains? grid
                                 (+ row (* len1 drow))
                                 (+ col (* len1 dcol))))
            (every? true? (map = pattern sampled-points))
            false)))

(defn scan-directions [grid [row col] pattern]
    (filter #(pattern-scans? grid [row col] % pattern) DIRECTIONS))

(defsolution day04 [input]
    (let [grid (parse-input input)
          good-scans (for [r (range (grid-height grid))
                           c (range (grid-width grid))]
                         (count (scan-directions grid [r c] PATTERN)))]
        [(reduce + good-scans)
         0]))
