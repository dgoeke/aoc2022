(ns aoc2022.day19
  (:require [aoc2022.core :as aoc]
            [clojure.string :as str]))

(defn parse [data]
  (->> (str/split-lines data)
       (map #(map read-string (re-seq #"\d+" %)))
       (map (partial map vector [:bp-n :cost-ore-ore :cost-clay-ore :cost-obs-ore
                           :cost-obs-clay :cost-geo-ore :cost-geo-obs]))
       (map (partial into {}))))

(defn in-minutes [{:as state :keys [ore-robots clay-robots obs-robots geo-robots]} mins]
  (-> state
      (update :minutes - mins)
      (update :ore + (* ore-robots mins))
      (update :clay + (* clay-robots mins))
      (update :geo + (* geo-robots mins))
      (update :obs + (* obs-robots mins))))

(defn next-states [{:as state :keys [ore-robots clay-robots obs-robots geo-robots minutes ore clay obs]}
                   {:keys [cost-ore-ore cost-clay-ore cost-obs-ore cost-obs-clay cost-geo-ore cost-geo-obs]}]
  (let [need-ore-bots  (-> (max cost-ore-ore cost-clay-ore cost-obs-ore cost-geo-ore)
                           (* minutes) (- ore) (/ minutes) Math/ceil int)
        need-clay-bots (-> cost-obs-clay (* minutes) (- clay) (/ minutes) Math/ceil int)
        need-obs-bots  (-> cost-geo-obs (* minutes) (- obs) (/ minutes) Math/ceil int)]
    (filter identity
            [(when (< ore-robots need-ore-bots)
               (let [mins (-> (- cost-ore-ore ore) (max 0) (/ ore-robots) Math/ceil int (max 1))]
                 (when (< mins minutes)
                   (-> (in-minutes state mins)
                       (update :ore-robots inc)
                       (update :ore - cost-ore-ore)
                       (update :steps conj [minutes :build-ore])))))
             (when (and (< clay-robots need-clay-bots) (< obs-robots need-obs-bots) (pos? ore-robots))
               (let [mins (-> (- cost-clay-ore ore) (max 0) (/ ore-robots) Math/ceil int (max 1))]
                 (when (< mins minutes)
                   (-> (in-minutes state mins)
                       (update :clay-robots inc)
                       (update :ore - cost-clay-ore)
                       (update :steps conj [minutes :build-clay])))))
             (when (and (< obs-robots need-obs-bots) (pos? ore-robots) (pos? clay-robots))
               (let [mins (max (-> (- cost-obs-ore ore) (max 0) (/ ore-robots) Math/ceil int)
                               (-> (- cost-obs-clay clay) (max 0) (/ clay-robots) Math/ceil int) 1)]
                 (when (< mins minutes)
                   (-> (in-minutes state mins)
                       (update :obs-robots inc)
                       (update :ore - cost-obs-ore)
                       (update :clay - cost-obs-clay)
                       (update :steps conj [minutes :build-obs])))))
             (when (and (pos? ore-robots) (pos? obs-robots))
               (let [mins (max (-> (- cost-geo-ore ore) (max 0) (/ ore-robots) Math/ceil int)
                               (-> (- cost-geo-obs obs) (max 0) (/ obs-robots) Math/ceil int) 1)]
                 (when (< mins minutes)
                   (-> (in-minutes state mins)
                       (update :geo-robots inc)
                       (update :ore - cost-geo-ore)
                       (update :obs - cost-geo-obs)
                       (update :steps conj [minutes :build-geo])))))
             (when (and (pos? geo-robots) (pos? minutes))
               (in-minutes state minutes))])))

(defn max-geodes [starting-state bp]
  (loop [[s & states] [starting-state]
         outcome      {:geo 0}]
    (if (nil? s)
      outcome
      (let [{[end] false rem true} (->> (next-states s bp)
                                        (sort-by :geo)
                                        (group-by (comp pos? :minutes)))]
        (recur (into states rem)
               (if (> (get end :geo 0) (:geo outcome))
                 end
                 outcome))))))

(def base-state {:ore-robots 1  :clay-robots 0 :obs-robots 0 :geo-robots 0
                 :ore        0  :clay        0 :obs        0 :geo        0
                 :minutes    24 :steps       []})

(defn part1 [data]
  (->> (parse data)
       (map (fn [{:as bp :keys [bp-n]}]
              (* bp-n (:geo (max-geodes base-state bp)))))
       (reduce max)))

(defn part2 [data]
  (->> (take 3 (parse data))
       (map #(:geo (max-geodes (assoc base-state :minutes 32) %)))
       (reduce *)))

;; (part1 (aoc/day 19)) ;=> 1147
;; (part2 (aoc/day 19)) ;=> 3080
