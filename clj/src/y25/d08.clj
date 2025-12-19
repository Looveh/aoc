(ns y25.d08
  (:require [clojure.math.combinatorics :as comb]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]
            [std]))

(defn dist [[x y z] [x' y' z']]
  (math/sqrt (+ (math/pow (- x x') 2)
                (math/pow (- y y') 2)
                (math/pow (- z z') 2))))

(defn parse-input [input]
  (let [boxes (for [line input
                    :let [[x y z] (str/split line #",")]]
                [(bigint x) (bigint y) (bigint z)])
        conns+dist (for [[a b] (comb/combinations boxes 2)]
                     [(dist a b) #{a b}])]
    [boxes (vec (sort-by first conns+dist))]))

(defn add-conn [circuits conn]
  (let [[a b] (vec conn)
        circ-a (first (filter #(contains? % a) circuits))
        circ-b (first (filter #(contains? % b) circuits))]
    (cond
      (and circ-a circ-b)
      (-> circuits
          (disj circ-a)
          (disj circ-b)
          (conj (set/union circ-a circ-b)))

      circ-a
      (-> circuits
          (disj circ-a)
          (conj (conj circ-a b)))

      circ-b
      (-> circuits
          (disj circ-b)
          (conj (conj circ-b a)))

      :else
      (conj circuits conn))))

(defn connect [conns+dist limit]
  (loop [[[_ conn] & conns+dist'] conns+dist
         circuits #{}
         n 0]
    (if (or (empty? conns+dist')
            (= n limit))
      circuits
      (recur conns+dist' (add-conn circuits conn) (inc n)))))

(defn pt1 [[_ conns+dist] limit]
  (->> (connect conns+dist limit)
       (map count)
       (sort)
       (reverse)
       (take 3)
       (apply *)))

(defn connect' [[boxes conns+dist]]
  (loop [[[_ conn] & conns+dist'] conns+dist
         circuits #{}]
    (let [circuits' (add-conn circuits conn)]
      (if (= (count (first circuits'))
             (count boxes))
        conn
        (recur conns+dist' circuits')))))

(defn pt2 [input]
  (let [last-conn (connect' input)
        [[x] [x']] (vec last-conn)]
    (* x x')))

(comment
  (time (pt1 (parse-input (std/read-input 25 8)) 1000))
  (time (pt2 (parse-input (std/read-input 25 8))))
  :-)
