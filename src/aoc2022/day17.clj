(ns aoc2022.day17
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(def width 7)
(def rocks   [:- :+ :> :| :.])
(def heights {:- 1 :+ 3 :> 3 :| 4 :. 2})

(defn parse-data [data] (map {\> :r \< :l} (str/trim data)))

(defn piece-locs [id x y]
  (case id
    :- #{ [x y] [(inc x) y] [(+ x 2) y] [(+ x 3) y] }
    :+ #{ [(inc x) y] [x (dec y)] [(inc x) (dec y)] [(+ x 2) (dec y)] [(inc x) (- y 2)] }
    :> #{ [(+ x 2) y] [(+ x 2) (dec y)] [x (- y 2)] [(inc x) (- y 2)] [(+ x 2) (- y 2)] }
    :| #{ [x y] [x (dec y)] [x (- y 2)] [x (- y 3)]}
    :. #{ [x y] [(inc x) y] [x (dec y)] [(inc x) (dec y)]}))

(defn place-rock [board rock x y]
  (set/union board (piece-locs rock x y)))

(defn valid-loc? [board rock x y]
  (let [locs (piece-locs rock x y)]
    (and (empty? (set/intersection board locs))
         (every? #(< -1 % width) (map first locs))
         (every? nat-int? (map second locs)))))

(defn simulate [{:as state :keys [level wind board]} rock]
  (let [[board wind x y]
        (loop [[w & ws] wind x 2 y (+ level 3 (heights rock))]
          (case w
            :l (if (valid-loc? board rock (dec x) y)
                 (recur ws (dec x) y)
                 (recur ws x y))
            :r (if (valid-loc? board rock (inc x) y)
                 (recur ws (inc x) y)
                 (recur ws x y))
            :d (if-not (valid-loc? board rock x (dec y))
                 [(place-rock board rock x y) ws x y]
                 (recur ws x (dec y)))))]
    (-> state
        (assoc :board board)
        (assoc :level (max level y))
        (assoc :wind wind))))

(defn part1 [data]
  (inc (:level (reduce simulate
                       {:level -1 :board #{} :wind (interpose :d (cycle data))}
                       (take 2022 (cycle rocks))))))

(defn board-hash [{:keys [board level last-rock]} rows]
  (->> board
       (filter (fn [[_ y]] (>= y (- level rows))))
       (map (fn [[x y]] (vector x (- level y))))
       set hash))

(defn find-cycle [data rocks-count]
  (reduce (fn [{:as acc :keys [state hashes n-rocks]} rock]
            (let [{:as state :keys [level]} (simulate state rock)
                  h                         (board-hash state 20)
                  level                     (inc level)
                  [old-n-rocks old-height]  (hashes h)]
              (if (and old-height (not= old-height level)
                       (zero? (mod (- rocks-count old-n-rocks) (- n-rocks old-n-rocks))))
                (reduced [[old-n-rocks old-height] [n-rocks level]])
                (-> acc
                    (assoc :state state)
                    (assoc-in [:hashes h] [n-rocks level])
                    (update :n-rocks inc)))))
          {:state   {:level -1 :board #{} :wind (interpose :d (cycle data))}
           :hashes  {}
           :n-rocks 1}
          (take (min rocks-count 5000) (cycle rocks))))

(defn part2 [data]
  (let [[[rocks1 height1] [rocks2 height2]] (find-cycle data 1000000000000)]
    (+ height1 (* (quot (- 1000000000000 rocks1)
                        (- rocks2 rocks1))
                  (- height2 height1)))))

(assert (= 3159          (-> (aoc/day 17) parse-data part1)))
(assert (= 1566272189352 (-> (aoc/day 17) parse-data part2)))
