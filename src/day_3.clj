(ns day-3
  (:require [core :refer [read-input-file]]
            [clojure.string :as str]))

(defn coordinate->adjacent-coordinates
  [coordinate])

(defn find-number-groups [s]
  (re-seq #"\d+" s))


(defn substring-index [s sub]
  (.indexOf s sub))


(defn replace-chunk [s chunk replacement]
  (str/replace-first s (re-pattern chunk) (apply str (repeat (count chunk) replacement))))

(defn parse-number-groups [row]
  (->> row
       find-number-groups
       (reduce (fn [{:as acc :keys [remaining]} chunk]

                 (let [start (substring-index remaining chunk)
                       end   (+ -1 start (count chunk))
                       group (-> {:val chunk}
                                 (assoc :start start :end end)
                                 (update :val parse-long))]

                   (-> acc
                       (update :groups conj group)
                       (update :remaining #(replace-chunk % chunk ".")))))
               {:remaining row :groups []})
       :groups))


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
  (def schematic (read-input-file "0301.txt"))

  (def number-groups (apply concat (remove empty? (map-indexed (fn [i row]
                                                                 (->> row
                                                                      parse-number-groups
                                                                      (map (fn [group]
                                                                             (assoc group :y i)))))
                                                               schematic))))

  (def symbol-coordinates (->> schematic
                               (map-indexed (fn [i row]
                                              (->> row
                                                   row->symbol-indices
                                                   (map (fn [j]
                                                          {:y i :x j})))))
                               (apply concat)
                               (reduce (fn [acc {:keys [y x]}]
                                         (assoc-in acc [y x] true)) {})))

  (->> number-groups
       (filter (fn [number-group]
                 (some (fn [adjacent-coord]
                         (let [{:keys [x y]} adjacent-coord]
                           (get-in symbol-coordinates [y x])))
                       (number-group->adjacent-coordinates number-group))))
       (map :val)
       (apply +))

  
  )
  