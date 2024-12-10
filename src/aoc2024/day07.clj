(ns aoc2024.day07
    (:use aoc2024.driver))

(def line-pattern #"(\d+): (.*)")

(defn parse-line [line]
  (let [[_ head tail] (re-find line-pattern line)]
    {:test (Long/valueOf head), :terms (map #(Long/valueOf %) (clojure.string/split tail #" "))}))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn add-and-multiply [lhs rhs]
  [(+ lhs rhs) (* lhs rhs)])

(defn concat-nums [lhs rhs]
  (Long/valueOf (str lhs rhs)))

(defn add-multiply-and-concat [lhs rhs]
  [(+ lhs rhs) (* lhs rhs) (concat-nums lhs rhs)])

(defn solvable? [generator {test-value :test, terms :terms}]
  (loop [stack [[(first terms) (rest terms)]]]
    (let [[cur terms] (peek stack)]
      (cond
       (empty? stack) false
       (and (empty? terms) (= test-value cur)) true
       (empty? terms) (recur (pop stack))
       (> cur test-value) (recur (pop stack))
       :else (let [new-stack-items (map (fn [x] [x (rest terms)])
                                        (generator cur (first terms)))]
               (recur (reduce conj (pop stack) new-stack-items)))))))

(defsolution day07 [input]
  (let [equations (parse-input input)]
    [(reduce + (map :test (filter (partial solvable? add-and-multiply) equations)))
     (reduce + (map :test (filter (partial solvable? add-multiply-and-concat) equations)))]))
