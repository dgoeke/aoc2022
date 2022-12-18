(ns aoc2022.day18
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [data]
  (->> (str/split-lines data)
       (map #(->> (str/split % #",")))
       (map (partial mapv #(Integer/parseInt %)))))

(defn neighbors [[x y z]]
  #{[(dec x) y z] [(inc x) y z] [x (inc y) z] [x (dec y) z] [x y (inc z)] [x y (dec z)]})

(defn count-neighbor-faces [{:as acc :keys [seen]} cube]
  (-> acc
      (update :total + 6)
      (update :total - (* 2 (count (set/intersection (neighbors cube) seen))))
      (update :seen conj cube)))

(defn part1 [data]
  (:total (reduce count-neighbor-faces {:seen #{} :total 0} (parse data))))

(defn part2 [data]
  (let [p1-result   (reduce count-neighbor-faces {:seen #{} :total 0} (parse data))
        all-cubes   (set (for [x (range 20) y (range 20) z (range 20)] [x y z]))
        empty-cubes (loop [[cube & cubes] [[0 0 0]]
                           empty-cubes    (set/difference all-cubes (:seen p1-result))]
                      (cond (nil? cube)        empty-cubes
                            (empty-cubes cube) (recur (concat cubes (neighbors cube))
                                                      (disj empty-cubes cube))
                            :else              (recur cubes empty-cubes)))]
    (:total (reduce count-neighbor-faces p1-result empty-cubes))))

(assert (= 3636 (part1 (aoc/day 18))))
(assert (= 2102 (part2 (aoc/day 18))))
