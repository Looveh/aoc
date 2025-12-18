(ns y25.d02
  (:require [clojure.string :as str]
            [std]))

(def input
  (as-> (std/slurp-input 25 2) $
    (str/trim $)
    (str/split $ #",")
    (map #(str/split % #"-") $)
    (map #(do [(bigint (first %))
               (bigint (second %))])
         $)))

(defn invalid-id? [id]
  (let [s (str id)
        left (subs s 0 (/ (count s) 2))
        right (subs s (/ (count s) 2) (count s))]
    (= left right)))

(defn invalid-id?' [id]
  (let [s (str id)]
    (some true? (for [n (range (/ (count s) 2))
                      :let [s' (subs s 0 (inc n))
                            re (re-pattern (str s' "(" s' ")+"))]
                      :when (re-matches re s)]
                  true))))

(defn pt1 [input]
  (let [invalid-ids (for [[start stop] input
                          id (range start (inc stop))
                          :when (invalid-id? id)]
                      id)]
    (apply + invalid-ids)))

(defn pt2 [input]
  (let [invalid-ids (for [[start stop] input
                          id (range start (inc stop))
                          :when (invalid-id?' id)]
                      id)]
    (apply + invalid-ids)))

(comment
  (time (pt1 input))
  (time (pt2 input))
  :-)
