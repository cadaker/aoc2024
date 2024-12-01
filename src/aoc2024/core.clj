(ns aoc2024.core
  (:use aoc2024.driver)
  (:require aoc2024.listing)
  (:gen-class))

(defn usage []
  (println "Arguments: <solution-no>")
  (println "  where solution-no is day01 etc."))

(defn -main
  "Main runner for the AoC solutions"
  [& args]
  (if (= 1 (count args))
    (let [solution-name (first args)
          solution (solutions solution-name)
          input (slurp (clojure.string/join ["data/" solution-name ".in"]))]
      (if solution
        (let [[sol1 sol2] (solution input)]
          (println sol1)
          (println sol2))
        (usage)))
    (usage)))
