(ns aoc2024.day07
    (:use aoc2024.driver))

(def line-pattern #"(\d+): (.*)")

(defn parse-line [line]
  (let [[_ head tail] (re-find line-pattern line)]
    {:test (Long/valueOf head), :terms (map #(Long/valueOf %) (clojure.string/split tail #" "))}))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn solvable? [{test-value :test, terms :terms}]
  (loop [stack [[(first terms) (rest terms)]]]
    (let [[cur terms] (peek stack)]
      (cond
       (empty? stack) false
       (and (empty? terms) (= test-value cur)) true
       (empty? terms) (recur (pop stack))
       :else (recur
               (-> stack
                   (pop)
                   (conj [(+ (first terms) cur) (rest terms)])
                   (conj [(* (first terms) cur) (rest terms)])))))))

(defsolution day07 [input]
  (let [equations (parse-input input)]
    [(reduce + (map :test (filter solvable? equations)))]))
