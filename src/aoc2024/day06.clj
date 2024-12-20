(ns aoc2024.day06
    (:use aoc2024.driver aoc2024.grid aoc2024.dir))

(defn parse-input [input]
  (loop [chars input
         row 0
         col 0
         builder (make-gridbuilder)
         guard-pos nil]
    (if-let [ch (first chars)]
      (cond (= ch \newline) (recur
                              (rest chars)
                              (inc row)
                              0
                              (gridbuilder-eol builder)
                              guard-pos)
            (= ch \^) (recur
                        (rest chars)
                        row
                        (inc col)
                        (gridbuilder-push builder \.)
                        [row col])
            :else (recur
                    (rest chars)
                    row
                    (inc col)
                    (gridbuilder-push builder ch)
                    guard-pos))
      [(gridbuilder-finish builder) {:pos guard-pos, :dir :up}])))

(defn step-guard [grid {[row col] :pos, dir :dir}]
  (let [[drow dcol] (step-in dir)
        next-pos [(+ row drow) (+ col dcol)]]
    (cond (not (grid-containsp? grid next-pos)) nil
          (= (grid-getp grid next-pos) \.) {:pos next-pos, :dir dir}
          :else {:pos [row col], :dir (turn-cw dir)})))

(defn guard-path [grid guard]
  (take-while some? (iterate #(step-guard grid %) guard)))

(defn guard-coords [grid guard]
  (into #{} (map :pos (guard-path grid guard))))

(defn path-loops? [guard-path]
  (loop [path guard-path
         visited #{}]
    (cond (nil? (seq path)) false
          (contains? visited (first path)) true
          :else (recur (rest path) (conj visited (first path))))))

(defn tweaked-grids [grid guard-start]
  (let [guard-positions (set (map :pos (guard-path grid guard-start)))]
    (for [[row col] guard-positions
          :when (and (= \. (grid-get grid row col))
                     (not= [row col] (:pos guard-start)))]
      (grid-assoc grid row col \#))))

(defsolution day06 [input]
  (let [[grid guard] (parse-input input)]
    [(count (guard-coords grid guard))
     (count (filter #(path-loops? (guard-path % guard)) (tweaked-grids grid guard)))]))
