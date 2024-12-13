(ns aoc2024.day11_test
    (:require [clojure.test :refer :all])
    (:require [aoc2024.day11 :refer :all]))

(deftest test-evolve
  (testing "evolve basics"
           (is (= (evolve "0") ["1"]))
           (is (= (evolve "1") ["2024"]))
           (is (= (evolve "10") ["1" "0"]))
           (is (= (evolve "99") ["9" "9"]))
           (is (= (evolve "999") ["2021976"]))))

(deftest test-evolve-all
  (testing "cases above"
           (is (= (evolve-all ["0" "1" "10" "99" "999"])
                  ["1" "2024" "1" "0" "9" "9" "2021976"])))
  (testing "given example"
           (is (= (evolve-all ["125" "17"])
                  ["253000" "1" "7"]))))

(deftest test-evolve-times
  (testing "given example"
           (is (= (evolve-times 1 ["125" "17"])
                  ["253000" "1" "7"]))
           (is (= (evolve-times 2 ["125" "17"])
                  ["253" "0" "2024" "14168"]))
           (is (= (evolve-times 3 ["125" "17"])
                  ["512072" "1" "20" "24" "28676032"]))
           (is (= (evolve-times 6 ["125" "17"])
                  ["2097446912" "14168" "4048" "2" "0" "2" "4" "40" "48" "2024" "40" "48" "80" "96" "2" "8" "6" "7" "6" "0" "3" "2"]))))
