(ns aoc2024.day10
    (:use aoc2024.driver aoc2024.grid))

(defn parse-input [input]
    (gridbuilder-finish
     (reduce (fn [builder ch]
                 (if (= ch \newline)
                     (gridbuilder-eol builder)
                     (gridbuilder-push builder (Integer/valueOf (str ch)))))
             (make-gridbuilder)
             input)))

(defn next-positions [grid [row col]]
    (let [val (grid-get grid row col)]
        (filter (fn [p]
                    (and (grid-containsp? grid p)
                         (= (inc val) (grid-getp grid p))))
                [[row (inc col)]
                 [row (dec col)]
                 [(inc row) col]
                 [(dec row) col]])))

(defn scan-trailhead [grid p]
    (loop [stack [p]
           end-positions #{}]
        (if (empty? stack)
            end-positions
            (let [pos (peek stack)
                  stack* (pop stack)]
                (if (= (grid-getp grid pos) 9)
                    (recur
                        stack*
                        (conj end-positions pos))
                    (recur
                        (reduce conj stack* (next-positions grid pos))
                        end-positions))))))

(defn trailheads [grid]
    (for [row (range (grid-height grid))
          col (range (grid-width grid))
          :when (= 0 (grid-get grid row col))]
        [row col]))

(defsolution day10 [input]
    (let [grid (parse-input input)]
        [(reduce + (map #(count (scan-trailhead grid %)) (trailheads grid)))
         0]))
