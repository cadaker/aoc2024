(ns aoc2024.day11
    (:use aoc2024.driver))

(defn tidy [s]
  (str (Long/valueOf s)))

(defn evolve [s]
  (let [n (count s)]
    (cond
     (= s "0") ["1"]
     (even? n) [(tidy (subs s 0 (/ n 2))) (tidy (subs s (/ n 2)))]
     :else [(str (* 2024 (Long/valueOf s)))])))

(defn evolve-all [stones]
  (reduce (fn [acc stone]
            (into acc (evolve stone)))
          []
          stones))

(defn evolve-times [n stones]
  (nth (iterate evolve-all stones) n))

(defsolution day11 [input]
  (let [stones (clojure.string/split (clojure.string/trimr input) #" ")]
  [(count (evolve-times 25 stones))
   ]))