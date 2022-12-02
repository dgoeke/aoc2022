(ns aoc2022.day2
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(def plays [:rock :paper :scissors])

(defn hand-score [opp me]
  (let [scores  (into {} (map vector plays (map inc (range 3))))
        winners (into {} (map vector plays (rest (cycle plays))))]
    (+ (scores me)
       (cond
         (= opp me)           3
         (= me (winners opp)) 6
         :else                0))))

(defn score-games [input parser score-transform]
  (->> (str/split-lines input)
       (map (juxt first #(nth % 2)))
       (map (partial map parser))
       (map score-transform)
       (map (partial apply hand-score))
       (reduce +)))

(defn part1 [data]
  (let [parser {\A :rock \B :paper \C :scissors
                \X :rock \Y :paper \Z :scissors}]
    (score-games data parser identity)))

(defn part2 [data]
  (let [parser   {\A :rock \B :paper \C :scissors
                  \X :lose \Y :draw  \Z :win}
        strategy (into {} (mapcat #(vector [[%1 :draw] %1] [[%1 :win] %2] [[%1 :lose] %3])
                                  plays (-> plays cycle rest) (->> plays cycle (drop 2))))
        score-xf (fn [[opp _ :as d]] [opp (strategy d)])]
    (score-games data parser score-xf)))

;; (part1 (aoc/day 2)) ;=> 10994
;; (part2 (aoc/day 2)) ;=> 12526
