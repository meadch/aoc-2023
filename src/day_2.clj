(ns day-2
  (:require [core :refer [read-input-file]]
            [clojure.string :as str]))

(defn game-str->game-id [game-str]
  (->> game-str
       (re-find #"^Game (\d+):.+")
       second
       parse-long))

(defn parse-digit
  "ie: 30 red => 30"
  [single-game-str]
  (->> single-game-str
       (re-find #".*?(\d+).*")
       second
       parse-long))

(defn parse-color
  "ie: 3 red => :red"
  [single-game-str]
  (->> single-game-str
       (re-find #".*(red|blue|green).*")
       second
       keyword))

(defn parse-game [single-game-string]
  (as-> single-game-string *
    (str/split * #",")
    (map (comp
          (juxt parse-color parse-digit)) *)
    (into {} *)))


(defn game-str->games [game-str]
  (as-> game-str *
    (re-find #"^Game \d+:(.+)$" *)
    (second *)
    (str/split * #";")
    (map parse-game *)))

(defn game-str->max-game [game-str]
  (apply merge-with max (game-str->games game-str)))

(defn possible-game? [limits game]
  (every? (fn [[color limit]]
            (<= (get game color 0) limit))
          limits))




(comment
  (def games (read-input-file "0201.txt"))

  (->> games
       (filter (comp (partial possible-game?
                              {:red 12 :green 13 :blue 14})
                     game-str->max-game))
       (map game-str->game-id)
       (apply +))
  )