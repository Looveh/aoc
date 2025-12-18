(ns y25.d05
  (:require [clojure.string :as str]
            [std]))

(def input
  (let [lines (std/read-input 25 5)
        ranges-block (take-while #(not= "" %) lines)
        ingredients-block (drop 1 (drop-while #(not= "" %) lines))
        ranges (for [range-line ranges-block
                     :let [[left right] (str/split range-line #"-")]]
                 [(bigint left) (bigint right)])
        ingredients (map bigint ingredients-block)]
    [(set ranges) ingredients]))

(defn fresh? [ingredient ranges]
  (some true? (for [[start end] ranges
                    :when (and (<= start ingredient)
                               (>= end ingredient))]
                true)))

(defn overlaps? [[start end] [start' _end']]
  (and (<= start start')
       (>= end start')))

(defn first-overlap [ranges]
  (first (for [a ranges
               b ranges
               :when (and (not= a b)
                          (overlaps? a b))]
           [a b])))

(defn compact-one [ranges [start end :as range-a] [start' end' :as range-b]]
  (-> ranges
      (disj range-a)
      (disj range-b)
      (conj [(min start start') (max end end')])))

(defn compact-all [ranges]
  (loop [ranges ranges]
    (let [[a b :as overlap] (first-overlap ranges)]
      (if-not overlap
        ranges
        (recur (compact-one ranges a b))))))

(defn pt1 [input]
  (let [[ranges ingredients] input]
    (count (filter #(fresh? % ranges) ingredients))))

(defn pt2 [input]
  (let [[ranges _] input
        compacted (compact-all ranges)]
    (apply + (for [[start end] compacted]
               (inc (- end start))))))

(comment
  (time (pt1 input))
  (time (pt2 input))
  :-)
