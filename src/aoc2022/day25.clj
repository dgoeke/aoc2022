(ns aoc2022.day25
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(def conv {\0 0, \1 1, \2 2, \- -1, \= -2})

(defn snafu->dec [s]
  (:total (reduce (fn [{:as result :keys [pos total]} n]
                    (-> (update result :pos inc)
                        (update :total + (* (conv n) (bigint (Math/pow 5 pos))))))
                  {:pos 0 :total 0}
                  (reverse s))))

(defn dec->snafu [d]
(loop [d d, result ""]
  (if (zero? d) result
      (case (mod d 5)
        0 (recur (quot d 5) (str \0 result))
        1 (recur (quot d 5) (str \1 result))
        2 (recur (quot d 5) (str \2 result))
        3 (recur (quot (+ 2 d) 5) (str \= result))
        4 (recur (quot (inc d) 5) (str \- result))))))

(defn part1 [data]
  (->> (str/split-lines data)
       (map snafu->dec)
       (reduce +)
       dec->snafu))

;; (part1 (aoc/day 25)) ;=> "2-0=11=-0-2-1==1=-22"
