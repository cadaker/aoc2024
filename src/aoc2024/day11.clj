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

(defn evolve-memoized [lookup stone n]
  (let [answer (get lookup [stone n])]
    (cond
     (zero? n) [lookup 1]
     (some? answer) [lookup answer]
     :else (let [parts (evolve stone)
                 [lookup* answer*] (reduce (fn [[lookup* answer*] part]
                                             (let [[part-lookup part-answer] (evolve-memoized lookup* part (dec n))]
                                               [part-lookup (+ answer* part-answer)]))
                                           [lookup 0]
                                           parts)]
             [(assoc lookup* [stone n] answer*) answer*]))))

(defsolution day11 [input]
  (let [stones (clojure.string/split (clojure.string/trimr input) #" ")]
  [(count (evolve-times 25 stones))
   (loop [stones stones
          lookup {}
          total 0]
     (if (seq stones)
       (let [[lookup* n] (evolve-memoized lookup (first stones) 75)]
         (recur (rest stones) lookup* (+ total n)))
       total))
   ]))