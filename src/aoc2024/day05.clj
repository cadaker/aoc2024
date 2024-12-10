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

(defn set-order [rule-set a b]
    (contains? rule-set [a b]))

(defn valid-update? [rule-set update]
    (= update (sort (partial set-order rule-set) update)))

(defn fix-update [rule-set update]
    (sort (partial set-order rule-set) update))

(defn middle-page [update]
    (nth update (quot (count update) 2)))

(defsolution day05 [input]
    (let [{rules :rules, updates :updates} (parse-input input)
          rule-set (into #{} rules)
          valid-updates (filter #(valid-update? rule-set %) updates)
          invalid-updates (remove #(valid-update? rule-set %) updates)
          fixed-updates (map #(fix-update rule-set %) invalid-updates)]
        [(reduce + (map middle-page valid-updates))
         (reduce + (map middle-page fixed-updates))]))
