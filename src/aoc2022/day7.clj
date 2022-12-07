(ns aoc2022.day7
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn handle-ls [{:as state :keys [cwd]} results]
  (assoc-in state (cons :tree cwd)
            (->> results
                 (map #(str/split % #" "))
                 (map (fn [[l r]] [r (if (= "dir" l) 0 (Integer/parseInt l))]))
                 (into {}))))

(defn handle-cd [state new-dir]
  (condp = new-dir
    "/"  (assoc state :cwd [new-dir])
    ".." (update state :cwd (comp vec butlast))
    (update state :cwd conj new-dir)))

(defn parse-lines [state lines]
  (if (empty? lines)
    (:tree state)
    (let [cmd           (first lines)
          [results rem] (split-with #(not (str/starts-with? % "$")) (rest lines))
          new-state     (let [cmd (rest (str/split cmd #" "))]
                          (condp = (first cmd)
                            "ls" (handle-ls state results)
                            "cd" (handle-cd state (second cmd))))]
      (recur new-state rem))))

(defn dir-sizes [result path tree]
  (let [{:keys [new-result local-size]}
        (reduce-kv (fn [{:as accum :keys [new-result]} k v]
                     (if-not (map? v)
                       (update accum :local-size + v)
                       (let [ds (dir-sizes new-result (conj path k) v)]
                         (-> accum
                             (assoc :new-result ds)
                             (update :local-size + (get ds (conj path k)))))))
                   {:local-size 0 :new-result result}
                   tree)]
    (assoc new-result path local-size)))

(defn all-dir-sizes [data]
  (->> (str/split-lines data)
       (parse-lines {:cwd ["/"] :tree {} :sizes {}})
       ((partial dir-sizes {} []))
       vals))

(defn part1 [data]
  (->> (all-dir-sizes data)
       (filter #(> 100000 %))
       (reduce +)))

(defn part2 [data]
  (let [sizes    (sort (all-dir-sizes data))
        required (- 30000000 (- 70000000 (last sizes)))]
    (first (filter (partial < required) sizes))))

;; (part1 (aoc/day 7)) ;=> 1490523
;; (part2 (aoc/day 7)) ;=> 12390492
