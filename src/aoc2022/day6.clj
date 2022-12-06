(ns aoc2022.day6
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn signal-start [buffer-len data]
  (+ buffer-len (->> (str/trim data)
                     (partition buffer-len 1)
                     (map set)
                     (take-while #(< (count %) buffer-len))
                     count)))

(def part1 (partial signal-start 4))
(def part2 (partial signal-start 14))

;; (part1 (aoc/day 6)) ;=> 1640
;; (part2 (aoc/day 6)) ;=> 3613
