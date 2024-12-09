(ns aoc2024.day09_test
    (:require [clojure.test :refer :all])
    (:require [aoc2024.day09 :refer :all])
    (:import [aoc2024.day09 disk-entry]))

(deftest move-test
  (testing "moving to big space"
           (is (= (move-into-space
                   (disk-entry. nil 10 0)
                   (disk-entry. 0 3 10))
                  [(disk-entry. 0 3 0)
                   [(disk-entry. nil 7 3)]
                   []])))
  (testing "moving to small space"
           (is (= (move-into-space
                   (disk-entry. nil 3 0)
                   (disk-entry. 0 10 3))
                  [(disk-entry. 0 3 0)
                   []
                   [(disk-entry. 0 7 3)]])))
  (testing "fits just right"
           (is (= (move-into-space
                   (disk-entry. nil 3 0)
                   (disk-entry. 0 3 3))
                  [(disk-entry. 0 3 0)
                   []
                   []]))))

(deftest parse-test
  (testing "example input"
           (is (= (parse-input "2333133121414131402")
                  [(disk-entry. 0 2 0) (disk-entry. nil 3 2)
                   (disk-entry. 1 3 5) (disk-entry. nil 3 8)
                   (disk-entry. 2 1 11) (disk-entry. nil 3 12)
                   (disk-entry. 3 3 15) (disk-entry. nil 1 18)
                   (disk-entry. 4 2 19) (disk-entry. nil 1 21)
                   (disk-entry. 5 4 22) (disk-entry. nil 1 26)
                   (disk-entry. 6 4 27) (disk-entry. nil 1 31)
                   (disk-entry. 7 3 32) (disk-entry. nil 1 35)
                   (disk-entry. 8 4 36) (disk-entry. nil 0 40)
                   (disk-entry. 9 2 40)]))))

(deftest defrag-test
  (testing "basic examples"
           (is (= (defrag-disk (parse-input "122"))
                  [(disk-entry. 0 1 0) (disk-entry. 1 2 1)]))
           (is (= (defrag-disk (parse-input "223"))
                  [(disk-entry. 0 2 0) (disk-entry. 1 2 2) (disk-entry. 1 1 4)]))
           (is (= (defrag-disk (parse-input "233"))
                  [(disk-entry. 0 2 0) (disk-entry. 1 3 2)]))
           (is (= (defrag-disk (parse-input "172"))
                  [(disk-entry. 0 1 0) (disk-entry. 1 2 1)])))
  (testing "example input"
           (is (= (defrag-disk (parse-input "2333133121414131402"))
                  [(disk-entry. 0 2 0)
                   (disk-entry. 9 2 2)
                   (disk-entry. 8 1 4)
                   (disk-entry. 1 3 5)
                   (disk-entry. 8 3 8)
                   (disk-entry. 2 1 11)
                   (disk-entry. 7 3 12)
                   (disk-entry. 3 3 15)
                   (disk-entry. 6 1 18)
                   (disk-entry. 4 2 19)
                   (disk-entry. 6 1 21)
                   (disk-entry. 5 4 22)
                   (disk-entry. 6 1 26)
                   (disk-entry. 6 1 27)]))))

(deftest checksum-test
  (testing "basics"
           (is (= (checksum [(disk-entry. 0 1 0) (disk-entry. 1 2 1) (disk-entry. 2 3 3)])
                  (+ 0 1 2 6 8 10))))
  (testing "example"
           (is (= (checksum (defrag-disk (parse-input "2333133121414131402")))
                  1928))))
