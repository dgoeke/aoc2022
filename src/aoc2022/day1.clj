(ns aoc2022.day1
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn calories-per-elf [data]
  (->> (str/split data #"\n\n")
       (map str/split-lines)
       (map (partial map #(Integer/parseInt %)))
       (map (partial reduce +))))

(defn part1 [data]
  (apply max (calories-per-elf data)))

(defn part2 [data]
  (->> (calories-per-elf data)
       (sort #(compare %2 %1))
       (take 3)
       (reduce +)))

;; (part1 (aoc/day 1)) ;=> 69289
;; (part2 (aoc/day 1)) ;=> 205615
