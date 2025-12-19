(ns y25.d07
  (:require [std]
            [grid]))

(defn step [grid rays y]
  (loop [rays rays
         x 0
         cnt 0]
    (cond
      (= x (grid/width grid))
      [cnt rays]

      (and (get rays x)
           (= "^" (grid/at grid [x y])))
      (recur (-> rays
                 (assoc x false)
                 (assoc (dec x) true)
                 (assoc (inc x) true))
             (inc x)
             (inc cnt))

      :else
      (recur rays (inc x) cnt))))

(defn pt1 [grid]
  (let [[start-x _] (grid/pos-of grid "S")
        rays (-> (repeat (grid/width grid) false)
                 (vec)
                 (assoc start-x true))]
    (loop [rays rays
           y 0
           cnt 0]
      (if (= y (grid/height grid))
        cnt
        (let [[hits rays'] (step grid rays y)]
          (recur rays' (inc y) (+ cnt hits)))))))

(defn next-splitter [grid [x y]]
  (loop [y (+ y 2)]
    (cond
      (>= y (grid/height grid)) nil
      (= "^" (grid/at grid [x y])) [x y]
      :else (recur (+ y 2)))))

(def splits-cnt
  (memoize
   (fn [grid p]
     (if-let [[x y] (next-splitter grid p)]
       (+ (splits-cnt grid [(dec x) y])
          (splits-cnt grid [(inc x) y]))
       1))))

(defn pt2 [grid]
  (let [[start-x _] (grid/pos-of grid "S")]
    (splits-cnt grid [start-x 0])))

(comment
  (time (pt1 (std/read-grid 25 7)))
  (time (pt2 (std/read-grid 25 7)))
  :-)
