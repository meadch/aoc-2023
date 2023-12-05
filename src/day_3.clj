(ns day-3
  (:require [core :refer [read-input-file]]
            [clojure.string :as str]))

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

(defn row->gear-indices [row]
  (->> row
       (map-indexed (fn [i c]
                      (when (re-find #"\*" (str c))
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

(defn number-group->coords [number-group]
  (let [{:keys [y start end val]} number-group
        id (random-uuid)]
    (map (fn [x] {:y y :x x :val val :id id}) (range start (inc end)))))

(defn gear->number-groups [number-coords gear]
  (->> (adjacent-coordinates gear)
       (map (fn [{:keys [x y]}]
              (get-in number-coords [y x])))

       (remove nil?)
       (group-by :id)
       (#(update-vals % first))
       vals))

(comment
  ;; part 1
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

  ;; answer
  
  (->> number-groups
       (filter (fn [number-group]
                 (some (fn [adjacent-coord]
                         (let [{:keys [x y]} adjacent-coord]
                           (get-in symbol-coordinates [y x])))
                       (number-group->adjacent-coordinates number-group))))
       (map :val)
       (apply +))


  )

(comment
  ;; part 2
  (def schematic (read-input-file "0301.txt"))

  (def number-coords (->> schematic
                          (map-indexed (fn [i row]
                                         (->> row
                                              parse-number-groups
                                              (map (fn [group]
                                                     (assoc group :y i))))))
                          (remove empty?)
                          (apply concat)
                          (mapcat number-group->coords)
                          (reduce (fn [acc {:keys [y x] :as item}]
                                    (assoc-in acc [y x] item)) {})))

  (def potential-gears (->> schematic
                            (map-indexed (fn [i row]
                                           (->> row
                                                row->gear-indices
                                                (map (fn [j]
                                                       {:y i :x j})))))
                            (apply concat)))

  ;; answer
  (->> potential-gears
       (map
        (fn [gear]
          (let [adjacent-groups (gear->number-groups number-coords gear)]
            (when (= 2 (count adjacent-groups))
              (apply * (map :val adjacent-groups))))))
       (remove nil?)
       (apply +))
  
  )
  