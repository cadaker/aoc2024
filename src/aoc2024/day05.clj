(ns aoc2024.day05
    (:use aoc2024.driver))

(def rule-pattern #"(\d+)\|(\d+)")

(defn parse-updates [lines]
    (map (fn [line]
             (map #(Integer/valueOf %) (clojure.string/split line #",")))
         lines))

(defn parse-input [input]
    (loop [lines (clojure.string/split-lines input)
           rules []]
        (if (empty? (first lines))
            {:rules rules, :updates (parse-updates (rest lines))}
            (let [[_ a b] (re-find rule-pattern (first lines))]
                (recur
                    (rest lines)
                    (conj rules [(Integer/valueOf a) (Integer/valueOf b)]))))))

(defn valid-update? [rule-set update]
    (let [ordered-pairs (for [n (range (count update))
                              m (range (inc n) (count update))]
                            [(nth update n) (nth update m)])]
        (not-any? (fn [[a b]] (contains? rule-set [b a])) ordered-pairs)))

(defn middle-page [update]
    (nth update (quot (count update) 2)))

(defsolution day05 [input]
    (let [{rules :rules, updates :updates} (parse-input input)
          rule-set (into #{} rules)
          valid-updates (filter #(valid-update? rule-set %) updates)]
        [(reduce + (map middle-page valid-updates))
         0]))
