(ns y25.d03
  (:require [std]))

(def input
  (->> (std/read-input 25 3)
       (map #(map (comp bigint str) %))))

(defn max-joltage [batteries n]
  (loop [batteries batteries
         n n
         nums []]
    (if (= n 0)
      (bigint (apply str nums))
      (let [m (apply max (drop-last (dec n) batteries))]
        (recur (drop 1 (drop-while #(not= m %) batteries))
               (dec n)
               (conj nums m))))))

(defn pt1 [input]
  (apply + (for [batteries input]
             (max-joltage batteries 2))))

(defn pt2 [input]
  (apply + (for [batteries input]
             (max-joltage batteries 12))))

(comment
  (time (pt1 input))
  (time (pt2 input))
  :-)
