(ns aoc2022.day21
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn parse-expr [[name val]]
  (let [[a b c] (map read-string (str/split (str/trim val) #" "))]
    [(read-string name) (if (nil? b) a (list b a c))]))

(defn reduce' [data k]
  (when-let [v (data k)]
    (if-not (coll? v)
      v
      (let [[op arg1 arg2] v]
        (list op (reduce' data arg1) (reduce' data arg2))))))

(def inverses {'+ '- '- '+ '* '/ '/ '*})
(def assoc? #{'+ '*})

(defn solve-equality [expr]
  (let [[_ arg1 arg2] expr
        res1          (try (eval arg1) (catch Throwable _ nil))
        res2          (try (eval arg2) (catch Throwable _ nil))
        [lhs rhs]     (if res1 [res1 arg2] [res2 arg1])]
    (loop [lhs lhs rhs rhs]
      (if (= rhs 'x)
        lhs
        (let [[op arg1 arg2] rhs
              res1           (try (eval arg1) (catch Throwable _ nil))
              res2           (try (eval arg2) (catch Throwable _ nil))]
          (cond
            (nil? res1) (recur (eval (list (inverses op) lhs res2)) arg1) ; 10 = x/2  =>  eval(10*2) = x
            (assoc? op) (recur lhs (list op arg2 res1))                   ; 10 = 2*x  =>  10 = x*2
            :else       (recur res1 (list (inverses op) arg2 lhs))))))))  ; 10 = 2/x  =>  2 = x*10

(defn parse [data]
  (->> (str/split-lines data)
       (map #(str/split % #":"))
       (map parse-expr)
       (into {})))

(defn part2 [data]
  (let [data (-> (parse data)
                 (update 'root #(conj (rest %) '=))
                 (assoc 'humn 'x))
        expr (reduce' data 'root)]
    (solve-equality expr)))

(defn part1 [data]
  (eval (reduce' (parse data) 'root)))

;; (part1 (aoc/day 21)) ;=> 194058098264286
;; (part2 (aoc/day 21)) ;=> 3592056845086
