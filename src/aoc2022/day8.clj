(ns aoc2022.day8
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn parse-grid [data]
  (let [grid (->> (str/split-lines data)
                  (mapv (partial mapv #(- (int %) (int \0)))))
        size (count grid)]
    [grid size]))

(defn walk [grid size [y x] [dir-y dir-x] highest results]
  (let [val           (get-in grid [y x])
        higher?       (> val highest)
        edge?         (or (zero? x) (zero? y) (= x (dec size)) (= y (dec size)))
        [new-x new-y] [(+ x dir-x) (+ y dir-y)]
        results       (if (or higher? edge?) (conj results [y x]) results)]
    (if (or (>= new-y size) (>= new-x size) (< new-y 0) (< new-x 0))
      results
      (recur grid size [new-y new-x] [dir-y dir-x] (if higher? val highest) results))))

(defn from-left [grid size] (mapcat #(walk grid size [% 0] [0 1] 0 #{}) (range size)))
(defn from-right [grid size] (mapcat #(walk grid size [% (dec size)] [0 -1] 0 #{}) (range size)))
(defn from-top [grid size] (mapcat #(walk grid size [0 %] [1 0] 0 #{}) (range size)))
(defn from-bottom [grid size] (mapcat #(walk grid size [(dec size) %] [-1 0] 0 #{}) (range size)))

(defn part1 [data]
  (let [[grid size] (parse-grid data)]
    (count (set (concat (from-left grid size)
                        (from-right grid size)
                        (from-top grid size)
                        (from-bottom grid size))))))

(defn look [grid size [y x] [dir-y dir-x] max-size result]
  (let [[y x] [(+ y dir-y) (+ x dir-x)]]
    (if (or (>= y size) (>= x size) (< x 0) (< y 0))
      result
      (if-not (< (get-in grid [y x]) max-size)
        (inc result)
        (recur grid size [y x] [dir-y dir-x] max-size (inc result))))))

(defn part2 [data]
  (let [[grid size] (parse-grid data)
        positions   (for [y (range size) x (range size)] [y x])
        directions  [[-1 0] [0 -1] [1 0] [0 1]]]
    (apply max (map (fn [[y x]]
                      (apply * (map #(look grid size [y x] % (get-in grid [y x]) 0)
                                    directions)))
                    positions))))

;; (part1 (aoc/day 8)) ;=> 1785
;; (part2 (aoc/day 8)) ;=> 345168
