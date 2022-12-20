(ns aoc2022.day20
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn wiggle [result [_ n :as item]]
  (let [i      (.indexOf result item)
        sliced (into (subvec result 0 i) (subvec result (inc i)))
        size   (count sliced)
        ni     (mod (+ n i) size)]
    (apply conj (subvec sliced 0 ni) item (subvec sliced ni))))

(defn parse [data]
  (->> (str/split-lines data)
       (mapv read-string)
       (mapv vector (iterate inc 0))))

(defn part1 [data]
  (let [ns    (mapv second (reduce wiggle data data))
        start (.indexOf ns 0)]
    (apply + (map #(nth (cycle ns) (+ start %)) [1000 2000 3000]))))

(defn part2 [data]
  (let [data  (mapv (fn [[a b]] (vector a (* b 811589153))) data)
        f     #(reduce wiggle % data)
        ns    (mapv second (first (drop 10 (iterate f data))))
        start (.indexOf ns 0)]
    (apply + (map #(nth (cycle ns) (+ start %)) [1000 2000 3000]))))

;; (-> (aoc/day 20) parse part1) ;=> 10763
;; (-> (aoc/day 20) parse part2) ;=> 4979911042808
