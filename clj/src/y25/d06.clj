(ns y25.d06
  (:require [clojure.core.matrix :as cm]
            [clojure.string :as str]
            [std]
            [grid]))

(defn ->op [op]
  (case op
    "+" +
    "*" *))

(defn pt1 []
  (let [input (->> (std/read-input 25 6)
                   (map str/trim)
                   (map #(str/split % #"\s+"))
                   (cm/transpose)
                   (map reverse))]
    (apply + (for [[op & ops] input
                   :let [op' (->op op)
                         ops' (map bigint ops)]]
               (apply op' ops')))))

(defn empty-row? [row]
  (every? #(= " " %) row))

(defn pt2 []
  (let [grid (->> (std/read-grid 25 6)
                  (grid/transpose))
        blocks (->> (:grid grid)
                    (partition-by empty-row?)
                    (remove (partial every? empty-row?)))
        eqs (for [block blocks
                  :let [op (->op (last (first block)))
                        ops (map (fn [ss]
                                   (->> ss
                                        (drop-last)
                                        (apply str)
                                        (str/trim)
                                        (bigint)))
                                 block)]]
              (apply op ops))]
    (apply + eqs)))

(comment
  (time (pt1))
  (time (pt2))
  :-)
