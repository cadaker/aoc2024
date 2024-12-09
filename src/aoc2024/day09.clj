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

(defn can-insert-file? [{space-id :file-id, space-length :length, space-start :start}
                        {file-id :file-id, file-length :length, file-start :start}]
  (and (nil? space-id)
       (< space-start file-start)
       (>= space-length file-length)))

(defn insertion-point [disk-map file]
  (let [[head tail] (split-with #(not (can-insert-file? % file)) disk-map)]
    (if (seq tail)
      [head tail]
      nil)))

(defn fill-space [{file-id :file-id, length :length, :as file-entry}
                  {space-length :length, space-start :start, :as space-entry}]
  (let [new-file-entry (disk-entry. file-id length space-start)]
    (if (= length space-length)
      [new-file-entry []]
      [new-file-entry [(disk-entry. nil (- space-length length) (+ space-start length))]])))

(defn defrag-whole-files [disk-map]
  (loop [files-to-go (filter :file-id (reverse disk-map))
         holes (filter #(not (:file-id %)) disk-map)
         output []]
    (if (seq files-to-go)
      (let [next-file (first files-to-go)]
        (if-let [[head tail] (insertion-point holes next-file)]
          (let [[new-file new-holes] (fill-space next-file (first tail))]
            (recur
              (rest files-to-go)
              (-> (vec head) (into new-holes) (into (rest tail)))
              (conj output new-file)))
          (recur (rest files-to-go) holes (conj output next-file))))
      (reverse output))))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn checksum [entries]
  (reduce + 0 (map (fn [{file-id :file-id, length :length, start :start}]
                     (* file-id (- (triangle (+ start length -1)) (triangle (dec start)))))
                   (filter :file-id entries))))

(defsolution day09 [input]
  (let [disk-map (parse-input (clojure.string/trimr input))]
    [(checksum (defrag-disk disk-map))
     (checksum (defrag-whole-files disk-map))]))
