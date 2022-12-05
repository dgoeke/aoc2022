(ns aoc2022.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- load-resource [filename]
  (some->> filename io/resource slurp))

(defn day
  "Load input data from resources directory. Returns `nil`
   if file does not exist, or a string if it does.
    (day 1)                => day1.txt
    (day 1 :sample)        => day1-sample.txt
    (day 2 :sample :part2) => day2-sample-part2.txt"
  [n & args]
  (load-resource
    (str (str/join "-" (cons (str "day" n)
                             (map name args)))
         ".txt")))
