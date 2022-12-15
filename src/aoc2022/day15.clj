(ns aoc2022.day15
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn abs-diff [a b] (Math/abs (- a b)))
(defn taxi-dist [[x1 y1] [x2 y2]] (+ (abs-diff x1 x2) (abs-diff y1 y2)))

(defn read-input [data]
  (map #((fn [[x y bx by]]
           {:x      x :y y
            :beacon [bx by]
            :range  (taxi-dist [x y] [bx by])})
         (map read-string (re-seq #"-?\d+" %)))
       (str/split-lines data)))

(defn row-coverage [row {:keys [x y range]}]
  (let [cover (- range (abs-diff row y))
        start (- x cover)]
    (when (nat-int? cover)
      [start (-> (inc cover) (* 2) dec (+ start))])))

(defn covers? [pt {:keys [x y range]}] (<= (taxi-dist pt [x y]) range))

(defn corners [{:keys [x y range]}]
  [(+ x y range) (+ x (- y range)) (- x (- y range)) (- x (+ y range))])

(defn merge-ranges [ranges]
  (loop [[l r :as cur]              (first ranges)
         [[n-l n-r :as next] & rem] (rest ranges)
         result                     []]
    (if-not next
      (conj result cur)
      (if (<= n-l r)
        (recur [l (max r n-r)] rem result)
        (recur next rem (conj result cur))))))

(defn part1 [data row]
  (let [sensors   (read-input data)
        coverage  (->> (keep (partial row-coverage row) sensors)
                       sort merge-ranges
                       (map (partial apply abs-diff))
                       (reduce +))
        n-beacons (->> (map :beacon sensors)
                       (filter #(= row (second %)))
                       distinct count)]
    (- coverage n-beacons)))

(defn part2 [data max-x max-y]
  (let [sensors (read-input data)
        pts     (->> (mapcat corners sensors)
                     (mapcat (juxt dec inc))
                     sort)]
    (some identity
          (map (fn [[x1 x2]]
                 (let [d (abs-diff x1 x2)
                       x (+ (quot d 2) (min x1 x2))
                       y (* (quot d 2) (Integer/signum (- x1 x2)))]
                   (and (zero? (mod d 2))
                        (<= 0 y max-y) (<= 0 x max-x)
                        (every? false? (map (partial covers? [x y]) sensors))
                        (+ y (* x 4000000)))))
               (for [a pts b pts] [a b])))))

;; (part1 (aoc/day 15) 2000000)         ;=> 5144286, ~2ms
;; (part2 (aoc/day 15) 4000000 4000000) ;=> 10229191267339, ~1sec
