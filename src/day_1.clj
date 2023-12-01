(ns day-1
  (:require [core :as core]))

(defn letters->digit [val]
  (get {"one" "1"
        "two" "2"
        "three" "3"
        "four" "4"
        "five" "5"
        "six" "6"
        "seven" "7"
        "eight" "8"
        "nine" "9"} val val))


(defn combine-digits [capture val]
  (let [[_ a b] (re-find
                 (re-pattern (str ".*?" capture ".*" capture ".*"))
                 val)

        [a b] (if (empty? a)
                (repeat (last (re-find
                               (re-pattern (str ".*?" capture ".*"))
                               val)))

                [a b])]

    (parse-long
     (apply str (map letters->digit [a b])))))

(comment
  (def inputs (core/read-input-file "0101.txt"))

;; part 1
  (->> inputs
       (map (partial combine-digits "(\\d)"))
       (reduce +))

;; part 2
  (->> inputs
       (map (partial combine-digits "(\\d|one|two|three|four|five|six|seven|eight|nine)"))
       (reduce +))
  )



