(ns aoc2022.day22
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(def facing {\E 0, \S 1, \W 2, \N 3})
(def moves {\E [0 1], \S [1 0], \W [0 -1], \N [-1 0]})
(def turns {\E [\N \S], \S [\E \W], \W [\S \N], \N [\W \E]})
(def sides {[0 1] {\N [[3 0] 2 \E], \W [[2 0] 2 \E]}
            [0 2] {\N [[3 0] 1 \N], \E [[2 1] 0 \W], \S [[1 1] 0 \W]}
            [1 1] {\E [[0 2] 1 \N], \W [[2 0] 3 \S]}
            [2 0] {\N [[1 1] 2 \E], \W [[0 1] 2 \E]}
            [2 1] {\E [[0 2] 0 \W], \S [[3 0] 0 \W]}
            [3 0] {\E [[2 1] 1 \N], \S [[0 2] 3 \S], \W [[0 1] 3 \S]}})

(defn build-map [[lines dirs]]
  (let [grid (mapv #(vec (seq %)) lines)
        row  0
        col  (str/index-of (apply str (first grid)) ".")
        dirs (map #(if (re-find #"\d+" %) (parse-long %) %)
                  (re-seq #"\d+|[LR]" (first dirs)))]
    {:grid grid, :dirs dirs, :start [row col]}))

(defn grid-y [grid start i x]
  (loop [y start]
    (let [ch (get-in grid [y x] \space)]
      (if (= ch \space)
        (recur (+ y i))
        y))))

(defn wrap-flat [grid pos dir]
  (let [[y x]  pos
        newpos (case dir
                 \E [y (count (take-while #(= % \space) (grid y)))]
                 \W [y (dec (count (grid y)))]
                 \N [(grid-y grid (dec (count grid)) -1 x) x]
                 \S [(grid-y grid 0 1 x) x])]
    (if (= \# (get-in grid newpos)) pos newpos)))

(defn move-flat [grid pos dir num]
  (let [mv (moves dir)]
    (loop [step 0 pos pos]
      (if (= step num)
        pos
        (let [newpos (mapv + mv pos)]
          (case (get-in grid newpos \space)
            \# (recur (inc step) pos)
            \. (recur (inc step) newpos)
            (recur (inc step) (wrap-flat grid pos dir))))))))

(defn walk-flat-pt1 [{:keys [grid dirs start]}]
  (loop [[d & ds] dirs, dir \E, pos start]
    (cond
      (nil? d)    (conj pos (facing dir))
      (number? d) (recur ds dir (move-flat grid pos dir d))
      :else       (let [i (if (= d "L") 0 1)]
                    (recur ds (get-in turns [dir i]) pos)))))

(defn get-newpos [face side y x dir]
  (let [min-y (* 50 (first face))
        max-y (dec (* 50 (inc (first face))))
        min-x (* 50 (last face))
        max-x (dec (* 50 (inc (last face))))
        pos   [(facing dir) side]]
    (cond
      (= pos [0 0]) [(- max-y (mod y 50)) max-x]
      (= pos [1 0]) [(+ min-y (mod x 50)) max-x]
      (= pos [2 0]) [(+ min-y (mod y 50)) max-x]
      (= pos [3 0]) [(- min-y (mod x 50)) max-x]
      (= pos [0 1]) [max-y (+ min-x (mod y 50))]
      (= pos [1 1]) [max-y (- max-x (mod x 50))]
      (= pos [2 1]) [max-y (- max-x (mod y 50))]
      (= pos [3 1]) [max-y (+ min-x (mod x 50))]
      (= pos [0 2]) [(+ min-y (mod x 50)) min-x]
      (= pos [1 2]) [(- max-y (mod x 50)) min-x]
      (= pos [2 2]) [(- max-y (mod y 50)) min-x]
      (= pos [3 2]) [(+ min-y (mod x 50)) min-x]
      (= pos [0 3]) [min-y (- max-x (mod y 50))]
      (= pos [1 3]) [min-y (+ min-x (mod x 50))]
      (= pos [2 3]) [min-y (+ min-x (mod y 50))]
      (= pos [3 3]) [min-y (- max-x (mod x 50))])))

(defn wrap-cube [grid [y x :as pos] dir]
  (let [face              [(quot y 50) (quot x 50)]
        [nface side ndir] (get-in sides [face dir])
        npos              (get-newpos nface side y x dir)]
    (if (= \# (get-in grid npos)) [pos dir] [npos ndir])))

(defn move-cube [grid pos dir num]
  (loop [step 0, pos pos, dir dir]
    (cond
      (= step num) [pos dir]
      :else        (let [newpos (mapv + (moves dir) pos)]
                     (case (get-in grid newpos \space)
                       \# (recur (inc step) pos dir)
                       \. (recur (inc step) newpos dir)
                       (let [[npos ndir] (wrap-cube grid pos dir)]
                         (recur (inc step) npos ndir)))))))

(defn walk-cube-pt2 [{:keys [grid dirs start]}]
  (loop [[d & ds] dirs, dir \E, pos start]
    (cond
      (nil? d)    (conj pos (facing dir))
      (number? d) (let [[npos ndir] (move-cube grid pos dir d)]
                    (recur ds ndir npos))
      :else       (let [i (if (= d "L") 0 1)]
                    (recur ds (get-in turns [dir i]) pos)))))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       build-map))

(defn password [row col facing]
  (+ (* 1000 (inc row)) (* 4 (inc col)) facing))

(defn part1 [input] (apply password (walk-flat-pt1 (parse input))))
(defn part2 [input] (apply password (walk-cube-pt2 (parse input))))

;; (part1 (aoc/day 22)) ;=> 191010
;; (part2 (aoc/day 22)) ;=> 55364
