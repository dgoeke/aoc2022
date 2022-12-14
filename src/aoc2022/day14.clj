(ns aoc2022.day14
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn points-in-line [[[x1 y1] [x2 y2]]]
  (cond
    (= y1 y2) (map #(vector % y1) (range (min x1 x2) (inc (max x1 x2))))
    (= x1 x2) (map #(vector x1 %) (range (min y1 y2) (inc (max y1 y2))))))

(defn input->map [data]
  (into {} (map #(vector % \#)
                (reduce (fn [accum line]
                          (->> (map #(Integer/parseInt %) (re-seq #"\d+" line))
                               (partition 2)           ; (x, y) pairs
                               (partition 2 1)         ; ((x1,y1), (x2, y2)) pairs
                               (mapcat points-in-line) ; points in line with those endpoints
                               (concat accum)))
                        #{}
                        (str/split-lines data)))))

(defn simulate [board part2?]
  (let [bottom (cond-> (->> (keys board) (map second) (reduce max)) part2? inc)]
    (loop [board           board
           [sand-x sand-y] [500 0]
           total           0]
      (cond
        (and (not part2?) (> sand-y bottom))       total
        (and part2? (= (board [500 0]) \o))        total
        (and part2? (= sand-y bottom))             (recur (assoc board [sand-x sand-y] \o) [500 0] (inc total))
        (nil? (board [sand-x (inc sand-y)]))       (recur board [sand-x (inc sand-y)] total)
        (nil? (board [(dec sand-x) (inc sand-y)])) (recur board [(dec sand-x) (inc sand-y)] total)
        (nil? (board [(inc sand-x) (inc sand-y)])) (recur board [(inc sand-x) (inc sand-y)] total)
        :else                                      (recur (assoc board [sand-x sand-y] \o) [500 0] (inc total))))))

(defn part1 [data] (simulate (input->map data) false))
(defn part2 [data] (simulate (input->map data) true))

;; (part1 (aoc/day 14)) ;=> 683
;; (part2 (aoc/day 14)) ;=> 28821
