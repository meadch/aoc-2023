(ns core
  (:require [clojure.java.io :as io]))

(defn read-input-file [filename]
  (with-open [rdr (io/reader (str "resources/inputs/" filename))]
    (vec (line-seq rdr))))

