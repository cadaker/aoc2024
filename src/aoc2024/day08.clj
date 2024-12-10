(ns aoc2024.day08
    (:use aoc2024.driver)
    (:require clojure.set))

(defn parse-input [input]
  (loop [input input
         row 0
         col 0
         width nil
         antennas {}]
    (cond
     (empty? input) {:width width, :height row, :antennas antennas}
     (= (first input) \newline) (recur (rest input) (inc row) 0 col antennas)
     (= (first input) \.) (recur (rest input) row (inc col) width antennas)
     :else (recur (rest input) row (inc col) width (update antennas (first input) #(conj (or % []) [row col]))))))

(defn pair-antinodes [[r1 c1] [r2 c2]]
  (let [dr (- r1 r2), dc (- c1 c2)]
    [[(+ r1 dr) (+ c1 dc)]
     [(- r2 dr) (- c2 dc)]]))

(defn group-antinodes [points]
  (apply clojure.set/union
         (for [m (range (count points))
               n (range (inc m) (count points))]
           (into #{} (pair-antinodes (nth points m) (nth points n))))))

(defn is-inside? [height width [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row height)
       (< col width)))

(defsolution day08 [input]
  (let [{width :width, height :height, antennas :antennas} (parse-input input)
        antinode-groups (map group-antinodes (vals antennas))]
    [(count (filter (partial is-inside? height width) (apply clojure.set/union antinode-groups)))]))
