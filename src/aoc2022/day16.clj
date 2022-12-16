(ns aoc2022.day16
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

;; WIP: here be dragons
;; currently ~30s for both parts on m1 mac

(let [valves         (->> (str/split-lines (aoc/day 16))
                          (map (partial re-seq #"[A-Z]{2}|\d+"))
                          (map (fn [[id rate & exits]]
                                 [id (Integer/parseInt rate) exits]))
                          (sort-by second)
                          reverse)
      label->index   (->> (map first valves) (map-indexed #(vector %2 %1)) (into {}))
      n-valves       (count valves)
      adjacencies    (reduce (fn [acc [id _ exits]]
                               (assoc acc (label->index id) (map label->index exits)))
                             (vec (repeat n-valves []))
                             valves)
      rates          (mapv second valves)
      pos-rates-bits (->> rates (filter pos?) count (bit-shift-left 1))
      results        (reduce
                       (fn [results [t i x]]
                         (let [ii (bit-shift-left 1 i)
                               o  (cond-> (get-in results [t i x])
                                    (and (not (zero? (bit-and ii x))) (>= t 2))
                                    (max (+ (get-in results [(dec t) i (- x ii)]) (* (rates i) t))))]
                           (assoc-in results [t i x] (reduce (fn [o j] (max o (get-in results [(dec t) j x])))
                                                             o (adjacencies i)))))
                       (vec (repeat 30 (vec (repeat n-valves (vec (repeat pos-rates-bits 0))))))
                       (for [t (range 1 30)
                             i (range n-valves)
                             x (range pos-rates-bits)] [t i x]))
      start-idx      (label->index "AA")
      part1          (get-in results [29 start-idx (dec pos-rates-bits)])
      part2          (reduce (fn [best [x y]]
                               (if (zero? (bit-and x y))
                                 (max best (+ (get-in results [25 start-idx x])
                                              (get-in results [25 start-idx y])))
                                 best))
                             0
                             (for [x (range pos-rates-bits)
                                   y (range pos-rates-bits) :when (<= y x)] [x y]))]
  [part1 part2])
