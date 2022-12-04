(ns aoc2022.day4
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn count-filtered-sets [data filter-fn]
  (->> (str/split-lines data)
       (map #(str/split % #"[-,]"))
       (map (partial map #(Integer/parseInt %)))
       (map (fn [[a b c d]]
              (vector (set (range a (inc b))) (set (range c (inc d))))))
       (filter filter-fn)
       count))

(defn part1 [data]
  (count-filtered-sets data (fn [[a b]]
                              (or (set/subset? a b) (set/subset? b a)))))

(defn part2 [data]
  (count-filtered-sets data (fn [[a b]]
                              (seq (set/intersection a b)))))

;; (part1 (aoc/day 4)) ;=> 595
;; (part2 (aoc/day 4)) ;=> 952
