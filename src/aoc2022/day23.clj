(ns aoc2022.day23
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(def dirs [[-1 0] [1 0] [0 -1] [0 1]])

(defn west-neighbors [[y x]] (set (for [dy [-1 0 1]] [(+ y dy) (dec x)])))
(defn east-neighbors [[y x]] (set (for [dy [-1 0 1]] [(+ y dy) (inc x)])))
(defn north-neighbors [[y x]] (set (for [dx [-1 0 1]] [(dec y) (+ x dx)])))
(defn south-neighbors [[y x]] (set (for [dx [-1 0 1]] [(inc y) (+ x dx)])))

(def neighbor-fns [north-neighbors south-neighbors west-neighbors east-neighbors])
(defn neighbors [pos] (set (mapcat #(% pos) (take 4 neighbor-fns))))

(defn parse [data]
  (->> (str/split-lines data)
       (map (partial map-indexed vector))
       (map-indexed (fn [y line] (->> line
                                     (filter #(= \# (second %)))
                                     (map (fn [[x _]] (vector y x))))))
       (apply concat)
       (into #{})))

(defn propose-moves [elves offsets neighbor-fns]
  (reduce (fn [result elf]
            (let [move (if (empty? (set/intersection elves (neighbors elf)))
                         [0 0]
                         (->> (map vector offsets neighbor-fns)
                              (map (fn [[dir nf]] (vector dir (nf elf))))
                              (drop-while #(seq (set/intersection (second %) elves)))
                              ffirst (#(or % [0 0]))))]
              (update result (mapv + move elf) (fnil conj []) elf)))
          {} elves))

(defn move [elves proposed]
  (reduce (fn [elves [target [elf]]]
            (-> elves (disj elf) (conj target)))
          elves
          (filter #(= 1 (count (second %))) proposed)))

(defn round [{:as state :keys [elves dirs neighbor-fns]}]
  (let [proposed  (propose-moves elves (take 4 dirs) (take 4 neighbor-fns))
        new-elves (move elves proposed)]
    (-> state
        (assoc :moved? (not= elves new-elves))
        (assoc :elves new-elves)
        (update :dirs rest)
        (update :neighbor-fns rest))))

(defn bounds [{:keys [elves]}]
  (let [min-x (reduce min (map second elves))
        min-y (reduce min (map first elves))
        max-x (reduce max (map second elves))
        max-y (reduce max (map first elves))]
    (- (* (inc (- max-x min-x)) (inc (- max-y min-y))) (count elves))))

(defn part1 [data]
  (let [state {:elves (parse data) :dirs (cycle dirs) :neighbor-fns (cycle neighbor-fns)}]
    (bounds (first (drop 10 (iterate round state))))))

(defn part2 [data]
  (let [state {:elves (parse data) :dirs (cycle dirs) :neighbor-fns (cycle neighbor-fns) :moved? true}]
    (count (take-while :moved? (iterate round state)))))

;; (part1 (aoc/day 23)) ;=> 4045
;; (part2 (aoc/day 23)) ;=> 963
