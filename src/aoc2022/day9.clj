(ns aoc2022.day9
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(def dirs {"R" [1 0] "L" [-1 0] "U" [0 -1] "D" [0 1]})

(defn move-snake [[positions hx hy tx ty] [dx dy]]
  (let [[hx hy] [(+ hx dx) (+ hy dy)]
        [tx ty] (if-not (> (max (Math/abs (- hx tx)) (Math/abs (- hy ty))) 1)
                  [tx ty]
                  [(if (pos? (Math/abs (- hx tx))) (if (> hx tx) (inc tx) (dec tx)) tx)
                   (if (pos? (Math/abs (- hy ty))) (if (> hy ty) (inc ty) (dec ty)) ty)])]
    [(conj positions [tx ty]) hx hy tx ty]))

(defn move-snake-p2 [[positions snake len] move]
  (let [pairs   (map vector (range len) (range 1 len))
        [snake] (reduce (fn [[snake head?] [ih it]]
                          (let [[h t]           [(get snake ih) (get snake it)]
                                [_ hx hy tx ty] (move-snake [#{} (first h) (second h) (first t) (second t)]
                                                            (if head? move [0 0]))]
                            [(-> snake (assoc ih [hx hy]) (assoc it [tx ty])) false]))
                        [snake true] pairs)]
    [(conj positions (last snake)) snake len]))

(defn parse-moves [data]
  (->> (str/split-lines data)
       (map #(str/split % #" "))
       (mapcat (fn [[l r]] (repeat (Integer/parseInt r) (dirs l))))))

(defn part1 [data]
  (->> (parse-moves data)
       (reduce move-snake [#{} 0 0 0 0])
       first count))

(defn part2 [data]
  (let [len   10
        moves (parse-moves data)
        snake (vec (repeat len [0 0]))]
    (count (first (reduce move-snake-p2 [#{} snake len] moves)))))

;; (part1 (aoc/day 9)) ;=> 6190
;; (part2 (aoc/day 9)) ;=> 2516
