(ns y25.d09
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [std])
  (:import [java.awt Polygon]
           [java.awt.geom Area Rectangle2D$Double]))

(def input
  (for [line (std/read-input 25 9)]
    (map Long/parseLong (str/split line #","))))

(defn area [[[x y] [x' y']]]
  (* (inc (abs (- x x')))
     (inc (abs (- y y')))))

(defn pt1 [input]
  (->> (combo/combinations input 2)
       (map area)
       (apply max)))

(defn ->rect [[[x y] [x' y']]]
  (Rectangle2D$Double. (min x x') (min y y') (abs (- x x')) (abs (- y y'))))

(defn pt2 [input]
  (let [outer (Polygon.)]
    (doseq [[x y] input]
      (.addPoint outer x y))
    (->> (combo/combinations input 2)
         (sort-by area #(compare %2 %1))
         (filter #(.contains (Area. outer) (->rect %)))
         (first)
         (area))))

(comment
  (time (pt1 input))
  (time (pt2 input))
  :-)
