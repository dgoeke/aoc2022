(ns aoc2022.day11
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn parse-monkey-desc [worry-div data]
  (let [num          (-> (re-find #"Monkey (\d+):" data) second read-string)
        op           (second (re-find #"Operation: new = (.*?)\n" data))
        div-test     (-> (re-find #"Test: divisible by (\d+)\n" data) second read-string)
        true-target  (-> (re-find #"If true: throw to monkey (\d+)\n" data) second read-string)
        false-target (-> (re-find #"If false: throw to monkey (\d+)" data) second read-string)
        op-fn        ((fn [[a b c]]
                        (str/replace (str "#(" (str/join " " [b a c]) ")") #"old" "%"))
                      (str/split op #" "))
        items        (-> (re-find #"Starting items: (.*?)\n" data) second (str/split #","))]
    {:num         num
     :op-fn       (eval (read-string op-fn))
     :worry-div   worry-div
     :test-val    div-test
     :test-fn     #(zero? (mod % div-test))
     :targets     {true true-target false false-target}
     :items       (mapv (comp bigint read-string str/trim) items)
     :inspections 0}))

(defn single-monkey-round [monkeys n]
  (let [{:keys [items op-fn test-fn targets worry-div worry-lcm]} (monkeys n)]
    (if (empty? items)
      monkeys
      (let [[item]      items
            worry-level (mod (quot (op-fn item) worry-div) worry-lcm)
            target      (targets (test-fn worry-level))]
        (recur (-> monkeys
                   (update-in [n :items] (comp vec rest))
                   (update-in [n :inspections] inc)
                   (update-in [target :items] conj worry-level))
               n)))))

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
(defn lcm [& v] (reduce #(/ (* %1 %2) (gcd %1 %2)) v))

(defn read-monkeys [data worry-div]
  (let [monkeys (mapv (partial parse-monkey-desc worry-div) (str/split data #"\n\n"))
        lcm     (apply lcm (map :test-val monkeys))]
    (mapv #(assoc % :worry-lcm lcm) monkeys)))

(defn n-rounds [data rounds worry-div]
  (let [monkeys (read-monkeys data worry-div)]
    (->> (iterate #(reduce single-monkey-round % (range (count %))) monkeys)
         (drop rounds)
         first
         (map :inspections)
         sort reverse
         (take 2)
         (apply *))))

(defn part1 [data] (n-rounds data 20 3))
(defn part2 [data] (n-rounds data 10000 1))

;; (part1 (aoc/day 11)) ;=> 59272
;; (part2 (aoc/day 11)) ;=> 14952185856, ~400ms
