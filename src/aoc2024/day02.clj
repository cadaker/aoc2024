(ns aoc2024.day01
    (:use aoc2024.driver))

(defn parse [line]
  (let [parts (clojure.string/split line #"\s+")]
    (map #(Integer/valueOf %) parts)))

(defn increasing? [xs]
  (apply < xs))

(defn decreasing? [xs]
  (apply > xs))

(defn safe-step? [x1 x2]
  (<= -3 (- x1 x2) 3))

(defn safe? [xs]
  (let [pairs (partition 2 1 xs)]
    (and (or (increasing? xs) (decreasing? xs))
         (every? #(apply safe-step? %) pairs))))

(defsolution day02 [input]
  (let [lines (clojure.string/split-lines input)
        data (map parse lines)]
    [(count (filter safe? data))
     0]))
