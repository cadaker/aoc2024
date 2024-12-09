(ns aoc2024.day09
    (:use aoc2024.driver))

(defrecord disk-entry [file-id length start])

(defn is-file? [entry]
  (some? (:file-id entry)))

(defn parse-input [chars]
  (loop [chars chars
         is-file true
         pos 0
         next-file-id 0
         output []]
    (if (seq chars)
      (let [ch (str (first chars))
            n (Integer/valueOf ch)]
        (recur
          (rest chars)
          (not is-file)
          (+ pos n)
          (if is-file (inc next-file-id) next-file-id)
          (conj output (disk-entry. (if is-file next-file-id nil) n pos))))
      output)))

(defn move-into-space [space file]
  "Move the file from the file entry into the space entry.
  Return a structure [output [new-fronts...] [new-ends...]]"
  (let [{space-len :length, space-start :start} space
        {file-id :file-id, file-len :length, file-start :start} file]
    (cond
     (> space-len file-len) [(disk-entry. file-id file-len space-start)
                             [(disk-entry. nil (- space-len file-len) (+ space-start file-len))]
                             []]
     (< space-len file-len) [(disk-entry. file-id space-len space-start)
                             []
                             [(disk-entry. file-id (- file-len space-len) file-start)]]
     :else [(disk-entry. file-id file-len space-start)
            []
            []])))

(defn defrag-disk [disk-map]
  (loop [disk-map disk-map
         reverse-map (reverse disk-map)
         output []]
    (let [entry (first disk-map)
          back-entry (first reverse-map)]
      (cond
       (> (:start entry) (:start back-entry)) output
       (= (:start entry) (:start back-entry)) (conj output back-entry)
       (is-file? entry) (recur (rest disk-map) reverse-map (conj output entry))
       (not (is-file? back-entry)) (recur disk-map (rest reverse-map) output)
       (zero? (:length entry)) (recur (rest disk-map) reverse-map output)
       :else (let [[moved new-fronts new-ends] (move-into-space entry back-entry)]
               (recur
                 (concat new-fronts (rest disk-map))
                 (concat new-ends (rest reverse-map))
                 (conj output moved)))))))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn checksum [entries]
  (reduce + 0 (map (fn [{file-id :file-id, length :length, start :start}]
                     (* file-id (- (triangle (+ start length -1)) (triangle (dec start)))))
                   entries)))

(defsolution day09 [input]
  (let [disk-map (parse-input (clojure.string/trimr input))]
    [(checksum (defrag-disk disk-map))
     0]))
