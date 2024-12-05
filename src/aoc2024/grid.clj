(ns aoc2024.grid)

(defrecord Grid [data width])

(defn grid-width [grid] (:width grid))

(defn grid-height [grid] (quot (count (:data grid)) (grid-width grid)))

(defn grid-contains? [grid row col]
  (and (<= 0 col (dec (grid-width grid)))
       (<= 0 row (dec (grid-height grid)))))

(defn grid-containsp? [grid [row col]]
  (grid-contains? grid row col))

(defn grid-get [grid row col]
  (let [index (+ (* row (grid-width grid)) col)]
    (get (:data grid) index)))

(defn grid-getp [grid [row col]]
  (grid-get grid row col))

(defrecord GridBuilder [data width])

(defn make-gridbuilder []
  (->GridBuilder [] nil))

(defn gridbuilder-push [grid-builder x]
  (update-in grid-builder [:data] #(conj % x)))

(defn gridbuilder-eol [grid-builder]
  (let [width (:width grid-builder)
        item-count (count (:data grid-builder))]
    (cond
     (nil? width) (assoc grid-builder :width item-count)
     (zero? (rem item-count width)) grid-builder
     :else (throw (Exception. "inconsistent line lengths")))))

(defn gridbuilder-finish [grid-builder]
  (let [builder (gridbuilder-eol grid-builder)]
    (->Grid (:data builder) (:width builder))))
