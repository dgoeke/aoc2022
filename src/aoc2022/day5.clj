(ns aoc2022.day5
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn parse-boxes [boxes]
  (let [indices (map inc (range 0 100 4))]
    (->> (str/split-lines boxes)
         (map (fn [line] (map #(when (<= % (count line)) (nth line %)) indices)))
         (apply mapv vector)
         (filter (comp not (partial every? nil?)))
         (map (comp reverse rest reverse))
         (mapv (partial filter #(some-> % (not= \space)))))))

(defn move-boxes-p1 [stacks [from to]]
  (let [item (first (nth stacks from))]
    (-> stacks
        (update from rest)
        (update to conj item))))

(defn move-boxes-p2 [stacks [n from to]]
  (let [[from to]   [(dec from) (dec to)]
        items       (take n (nth stacks from))]
    (-> stacks
        (update from (partial drop n))
        (update to (partial concat items)))))

(defn run-all-moves [stacks moves mover-fn]
  (if (empty? moves)
    stacks
    (recur (mover-fn stacks (first moves)) (rest moves) mover-fn)))

(defn parse-input [data mover-fn post-parse-fn]
  (let [[boxes moves] (str/split data #"\n\n")
        stacks        (parse-boxes boxes)
        movelist      (->> (str/split-lines moves)
                           (map (partial re-seq #"\d+") )
                           (mapv (partial map read-string))
                           (post-parse-fn))]
    (apply str (map first (run-all-moves stacks movelist mover-fn)))))

(defn part1 [data]
  (parse-input data move-boxes-p1
               (partial mapcat (fn [[num from to]] (repeat num [(dec from) (dec to)])))))

(defn part2 [data]
  (parse-input data move-boxes-p2 identity))

;; (part1 (aoc/day 5)) ;=> "VCTFTJQCG"
;; (part2 (aoc/day 5)) ;=> "GCFGLDNJZ"
