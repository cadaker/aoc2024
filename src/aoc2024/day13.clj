(ns aoc2024.day13
    (:use aoc2024.driver))

(def input-pattern #"Button A: X\+(\d+), Y\+(\d+)
Button B: X\+(\d+), Y\+(\d+)
Prize: X=(\d+), Y=(\d+)")

(defn as-int [s]
  (Long/valueOf s))

(defn parse-group [group]
  (let [[_ ax ay bx by px py] (re-find input-pattern group)]
    {:a [(as-int ax) (as-int ay)], :b [(as-int bx) (as-int by)], :p [(as-int px) (as-int py)]}))

(defn parse-input [input]
  (map parse-group (clojure.string/split input #"\n\n")))

(defn det [[ax ay] [bx by]]
  "Determinant, given the two column vectors"
  (- (* ax by) (* bx ay)))

(defn solve-unique [{a :a, b :b, p :p}]
  (let [d (det a b)
        na (/ (det p b) d)
        nb (/ (det a p) d)]
    (if (and (integer? na) (integer? nb))
      [na nb]
      nil)))

(defn solve-machine [{a :a, b :b, :as machine}]
  (let [determinant (det a b)]
    (if (= determinant 0)
      :tricky
      (solve-unique machine))))

(defn tokens [[na nb]]
  (+ (* 3 na) nb))

(defn adjust-machine [offset {a :a, b :b, [px py] :p}]
  {:a a, :b b, :p [(+ px offset) (+ py offset)]})

(def machine-offset 10000000000000)

(defsolution day13 [input]
  (let [machines (parse-input input)
        adjusted-machines (map (partial adjust-machine machine-offset) machines)]
    ; No tricky cases for either part!
    [(reduce + (map tokens (remove nil? (map solve-machine machines))))
     (reduce + (map tokens (remove nil? (map solve-machine adjusted-machines))))]))
