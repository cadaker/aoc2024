(ns aoc2024.dir)

;; This file assumes row/col coordinate systems.

(def dirs [:up :right :left :down])

(def turn-cw {:up :right, :right :down, :down :left, :left :up})

(def step-in {:up [-1 0], :right [0 1], :down [1 0], :left [0 -1]})
