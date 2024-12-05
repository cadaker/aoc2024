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

(defn has-x-mas? [grid [row col]]
    (let [row+ (inc row)
          row- (dec row)
          col+ (inc col)
          col- (dec col)]
        (and (grid-contains? grid row- col-)
             (grid-contains? grid row+ col+)
             (= (grid-get grid row col) \A)
             (or (and (= (grid-get grid row- col-) \M)
                      (= (grid-get grid row+ col+) \S))
                 (and (= (grid-get grid row- col-) \S)
                      (= (grid-get grid row+ col+) \M)))
             (or (and (= (grid-get grid row- col+) \M)
                      (= (grid-get grid row+ col-) \S))
                 (and (= (grid-get grid row- col+) \S)
                      (= (grid-get grid row+ col-) \M))))))

(defsolution day04 [input]
    (let [grid (parse-input input)
          height (grid-height grid)
          width (grid-width grid)
          good-scans (for [r (range width)
                           c (range height)]
                         (count (scan-directions grid [r c] PATTERN)))
          xmas-poses (for [r (range height)
                           c (range width)
                           :when (has-x-mas? grid [r c])]
                         [r c])]
        [(reduce + good-scans)
         (count xmas-poses)]))
