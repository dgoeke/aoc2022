(ns aoc2022.day12
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn parse-input [data]
  (let [data  (str/split-lines data)
        [w h] [(count (first data)) (count data)]
        {:keys [grid start end]}
        (reduce (fn [{:as accum :keys [pos]} ch]
                  (-> (update accum :pos inc)
                      (update :grid conj (- (int (condp = ch \S \a \E \z ch)) (int \a)))
                      (cond->
                          (= ch \S) (assoc :start pos)
                          (= ch \E) (assoc :end pos))))
                {:grid [] :pos 0}
                (str/join data))]
    [start end {:heights grid :width w :height h :len (count grid)}]))

(defn in-bounds? [{:keys [len]} pos] (and (>= pos 0) (< pos len)))
(defn steppable? [{:keys [heights]} to from] (<= (heights to) (inc (heights from))))

(defn neighbors [{:as grid :keys [width height]} pos]
  (when (in-bounds? grid pos)
    (filter (every-pred identity (partial in-bounds? grid) (partial steppable? grid pos))
            [(when-not (zero? (mod pos width)) (dec pos))
             (when-not (= (dec width) (mod pos width)) (inc pos))
             (when-not (zero? (quot pos width)) (- pos width))
             (when-not (= (dec height) (quot pos width)) (+ pos width))])))

(defn shortest-paths [{:as grid :keys [len]} end]
  (loop [results (assoc (vec (repeat len Integer/MAX_VALUE)) end 0)
         queue   (map #(vector % 1) (neighbors grid end))]
    (if (empty? queue)
      results
      (let [[[pos dist] & qs] queue]
        (if (< dist (results pos))
          (recur (assoc results pos dist)
                 (concat qs (map #(vector % (inc dist)) (neighbors grid pos))))
          (recur results qs))))))

(defn part1 [data]
  (let [[start end grid] (parse-input data)]
    (get (shortest-paths grid end) start)))

(defn part2 [data]
  (let [[_ end grid] (parse-input data)
        distances    (shortest-paths grid end)]
    (apply min (map distances (keep-indexed #(when (zero? %2) %1) (:heights grid))))))

;; (part1 (aoc/day 12)) ;=> 484
;; (part2 (aoc/day 12)) ;=> 478
