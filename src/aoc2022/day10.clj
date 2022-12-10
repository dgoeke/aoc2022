(ns aoc2022.day10
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn handle-cycle [{:as state :keys [cycle x]}]
  (let [scan-x (mod (dec cycle) 40)]
    (cond-> state
      (zero? (mod cycle 20))      (update :milestones conj [cycle (* cycle x)])
      (<= (dec x) scan-x (inc x)) (update :screen assoc (dec cycle) \#))))

(defn process [state [instr param]]
  (condp = instr
    "noop" (-> (handle-cycle state)
               (update :cycle inc))
    "addx" (let [p (Integer/parseInt param)]
             (-> (handle-cycle state)
                 (update :cycle inc)
                 handle-cycle
                 (update :cycle inc)
                 (update :x + p)))))

(defn run [data]
  (->> (str/split-lines data)
       (map #(str/split % #" "))
       (reduce process {:cycle  1 :x 1 :milestones []
                        :screen (vec (repeat 240 \space))})))

(defn part1 [data]
  (let [cycles (into {} (:milestones (run data)))]
    (reduce + (vals (select-keys cycles [20 60 100 140 180 220])))))

(defn part2 [data]
  (->> (:screen (run data))
       (partition 40)
       (map (comp println (partial apply str)))))

;; (part1 (aoc/day 10)) ;=> 14360
;; (part2 (aoc/day 10)) ;=> BGKAEREZ
