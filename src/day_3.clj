(ns day-3
  (:require [core :refer [read-input-file]]
            [clojure.string :as str]))

(defn coordinate->adjacent-coordinates
  [coordinate])

(defn find-number-groups [s]
  (re-seq #"\d+" s))


(defn substring-index [s sub]
  (.indexOf s sub))


(defn parse-number-groups [row]
  (->> row
       find-number-groups
       (map (comp
             #(update % :val parse-long)
             #(assoc % :end (+ -1 (:start %) (count (:val %))))
             #(assoc % :start (substring-index row (:val %)))
             (partial hash-map :val)))))

(defn row->symbol-indices [row]
  (->> row 
       (map-indexed (fn [i c]
                 (when-not (re-find #"\d|\." (str c))
                   i)))
       (remove nil?)))

(defn adjacent-coordinates [{:keys [x y]}]
  (let [deltas [-1 0 1]]
    (for [dx deltas, dy deltas
          :when (not (and (= dx 0) (= dy 0)))]
      {:x (+ x dx) :y (+ y dy)})))


(defn number-group->adjacent-coordinates [number-group]
  (let [{:keys [y start end]} number-group
        digit-coords (map (fn [x] {:y y :x x}) (range start (inc end)))]
    (->> digit-coords
         (map adjacent-coordinates)
         (apply concat)
         set
         (remove (set digit-coords)))))

(comment
  (def schematic (read-input-file "0301.sample.txt"))

  (def number-groups (apply concat (remove empty? (map-indexed (fn [i row]
                                                                 (->> row
                                                                      parse-number-groups
                                                                      (map (fn [group]
                                                                             (assoc group :y i)))))
                                                               schematic))))

  (->> number-groups
       
       
       (filter (fn [number-group]
                 (some (fn [adjacent-coord]
                         (let [{:keys [x y]} adjacent-coord]
                           (get-in symbol-coordinates [y x])))
                       (number-group->adjacent-coordinates number-group))))
       (map :val)
       (apply +))

  (def symbol-coordinates (->> schematic
                               (map-indexed (fn [i row]
                                              (->> row
                                                   row->symbol-indices
                                                   (map (fn [j]
                                                          {:y i :x j})))))
                               (apply concat)
                               (reduce (fn [acc {:keys [y x]}]
                                         (assoc-in acc [y x] true)) {})))
  )
  