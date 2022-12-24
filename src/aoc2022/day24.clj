(ns aoc2022.day24
  (:require [aoc2022.core :as aoc]
            [clojure.set :as set]
            [clojure.string :as str]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]))

(def north [-1 0])
(def south [1 0])
(def west [0 -1])
(def east [0 1])

(def input (str/split-lines (aoc/day 24)))
(def rows (- (count input) 2))
(def cols (- (count (first input)) 2))

(defn irange [x y] (range x (inc y)))
(def blizzard-directions {\> east \^ north \< west \v south})
(def rev-blizzard-directions (set/map-invert blizzard-directions))

(defn v+-wrap [[a b] [c d]]
  [(mod (+ a c) rows) (mod (+ b d) cols)])

(def blizzards
  (into {} (for [[row line] (mapv vector (iterate inc 0) input)
                 col        (range (count line))
                 :let       [character (nth line col)]
                 :when      (contains? blizzard-directions character)]
             [[(dec row) (dec col)] [(blizzard-directions character)]])))

(defn in-grid? [[x y :as pt]]
  (or (and (< -1 x rows) (< -1 y cols))
      (#{[-1 0] [rows (dec cols)]} pt)))

(defn neighbors [pt]
  (for [dir   [east west north south [0 0]]
        :let  [new-pt (mapv + pt dir)]
        :when (in-grid? new-pt)]
    new-pt))

(defn update-blizzards [bs]
  (apply merge-with into (for [[pt dirs] bs dir dirs] {(v+-wrap pt dir) [dir]})))

(def update-blizzards (memoize update-blizzards))
(def period (count (set (take (* rows cols) (iterate update-blizzards blizzards)))))
(def all-possible-blizzards (vec (take period (iterate update-blizzards blizzards))))

(def start-cell [-1 0])
(def end-cell [rows (dec cols)])
(def initial-state [start-cell 0])

(defn end? [target] (fn [[me _]] (= me target)))
(defn taxi-dist [[a b] [c d]] (+ (abs (- a c)) (abs (- b d))))
(defn lower-bound [target] (fn [[me _]] (taxi-dist me target)))

(defn next-state [[me blizzard-i]]
  (let [blizzard-i    (mod (inc blizzard-i) period)
        me-candidates (remove (nth all-possible-blizzards blizzard-i) (neighbors me))]
    (for [pt me-candidates] {:dest [pt blizzard-i]})))

(def one-way (alg/shortest-path next-state {:start-node   initial-state
                                            :end-node?    (end? end-cell)
                                            :heuristic-fn (lower-bound end-cell)}))

(defn part1 [] (:cost one-way))

(defn part2 []
  (let [{trip1 :end cost1 :cost} one-way
        {trip2 :end cost2 :cost} (alg/shortest-path next-state
                                                    {:start-node   trip1
                                                     :end-node?    (end? start-cell)
                                                     :heuristic-fn (lower-bound start-cell)})
        {trip3 :end cost3 :cost} (alg/shortest-path next-state
                                                    {:start-node   trip2
                                                     :end-node?    (end? end-cell)
                                                     :heuristic-fn (lower-bound end-cell)})]
    (+ cost1 cost2 cost3)))
