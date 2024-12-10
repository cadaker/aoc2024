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

(defn is-inside? [height width [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row height)
       (< col width)))

(defn pair-antinodes [[r1 c1] [r2 c2]]
  (let [dr (- r1 r2), dc (- c1 c2)]
    [[(+ r1 dr) (+ c1 dc)]
     [(- r2 dr) (- c2 dc)]]))

(defn all-antinodes [height width [r1 c1] [r2 c2]]
  (let [dr (- r1 r2), dc (- c1 c2)]
    (concat (take-while (partial is-inside? height width)
                        (iterate (fn [[r c]]
                                   [(+ r dr) (+ c dc)])
                                 [r1 c1]))
            (take-while (partial is-inside? height width)
                        (iterate (fn [[r c]]
                                   [(- r dr) (- c dc)])
                                 [r2 c2])))))

(defn group-antinodes [antinode-gen points]
  (apply clojure.set/union
         (for [m (range (count points))
               n (range (inc m) (count points))]
           (into #{} (antinode-gen (nth points m) (nth points n))))))

(defn count-and-filter [height width antinodes]
  (count (filter (partial is-inside? height width) antinodes)))

(defsolution day08 [input]
  (let [{width :width, height :height, antennas :antennas} (parse-input input)
        antinode-groups (map (partial group-antinodes pair-antinodes) (vals antennas))
        all-antinode-groups (map (partial group-antinodes (partial all-antinodes height width)) (vals antennas))]
    [(count-and-filter height width (apply clojure.set/union antinode-groups))
     (count-and-filter height width (apply clojure.set/union all-antinode-groups))]))
