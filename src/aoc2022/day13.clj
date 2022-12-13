(ns aoc2022.day13
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn check-order [a b]
  (cond
    (and (int? a) (int? b))   (compare a b)
    (and (coll? a) (coll? b)) (loop [[r & rs] (map check-order a b)]
                                (case r
                                  nil (- (count a) (count b))
                                  0   (recur rs)
                                  r))
    (coll? a)                 (check-order a [b])
    (coll? b)                 (check-order [a] b)))

(defn packets [data]
  (->> (str/split-lines data)
       (filter (comp not str/blank?))
       (map read-string)))

(defn part1 [data]
  (->> (partition 2 (packets data))
       (map (partial apply check-order))
       (keep-indexed #(when (neg? %2) (inc %1)))
       (reduce +)))

(defn part2 [data]
  (->> (concat (packets data) [2 6])
       (sort check-order)
       (keep-indexed #(when (#{2 6} %2) (inc %1)))
       (apply *)))

;; (part1 (aoc/day 13)) ;=> 6272
;; (part2 (aoc/day 13)) ;=> 22288
