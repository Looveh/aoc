(ns y25.d04
  (:require [std]
            [grid]))


(defn removable [grid]
  (for [pos (grid/positions grid)
        :when (= "@" (grid/at grid pos))
        :let [neighbors (grid/neighbors-vals* grid pos)]
        :when (> 4 (count (filter #(= "@" %) neighbors)))]
    pos))

(defn pt1 [grid]
  (count (removable grid)))

(defn pt2 [grid]
  (loop [grid grid
         removed-cnt 0]
    (let [to-remove (removable grid)]
      (if (empty? to-remove)
        removed-cnt
        (recur (grid/set-vals grid to-remove ".")
               (+ removed-cnt (count to-remove)))))))

(comment
  (time (pt1 (std/read-grid 25 4)))
  (time (pt2 (std/read-grid 25 4)))
  :-)
