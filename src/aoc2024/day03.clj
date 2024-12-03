(ns aoc2024.day03
    (:use aoc2024.driver))

(def MUL-PATTERN #"mul\((\d{1,3}),(\d{1,3})\)")
(def FULL-PATTERN #"(do\(\))|(don't\(\))|mul\((\d{1,3}),(\d{1,3})\)")

(defn parse-match [m]
  (let [do-part (nth m 1)
        dont-part (nth m 2)
        factor1 (nth m 3)
        factor2 (nth m 4)]
    (cond
     (not (empty? do-part)) :do
     (not (empty? dont-part)) :dont
     :else [(Integer/valueOf factor1) (Integer/valueOf factor2)])))

(defn run-commands [commands]
  (loop [commands commands
         multiplying true
         acc 0]
    (let [cmd (first commands)]
      (cond
       (nil? cmd) acc
       (= cmd :do) (recur (rest commands) true acc)
       (= cmd :dont) (recur (rest commands) false acc)
       (not multiplying) (recur (rest commands) false acc)
       :else (let [[a b] cmd]
               (recur (rest commands) true (+ acc (* a b))))))))

(defsolution day03 [input]
  (let [mul-matches (re-seq MUL-PATTERN input)
        factors (map (fn [m]
                       [(Integer/valueOf (nth m 1))
                        (Integer/valueOf (nth m 2))])
                     mul-matches)
        full-matches (re-seq FULL-PATTERN input)
        parsed (map parse-match full-matches)]
    [(reduce + (map (fn [[a b]] (* a b)) factors))
     (run-commands parsed)]))
