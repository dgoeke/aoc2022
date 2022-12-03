(ns aoc2022.day3
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn string-halves [s]
  (let [m (/ (count s) 2)]
    [(subs s 0 m) (subs s m)]))

(defn priority [c]
  (inc (cond
         (<= (int \A) (int c) (int \Z)) (+ 26 (- (int c) (int \A)))
         (<= (int \a) (int c) (int \z)) (- (int c) (int \a))
         :else                          nil)))

(defn search-bags [data partition-fn]
  (->> (str/split-lines data)
       partition-fn
       (map (partial map set))
       (map (partial apply set/intersection))
       (map first)
       (map priority)
       (reduce +)))

(defn part1 [data] (search-bags data (partial map string-halves)))
(defn part2 [data] (search-bags data (partial partition 3)))

;; (part1 (aoc/day 3)) ;=> 7997
;; (part2 (aoc/day 3)) ;=> 2545
