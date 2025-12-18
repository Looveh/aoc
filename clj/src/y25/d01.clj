(ns y25.d01
  (:require [std]))

(defn pt1 [input]
  (loop [[line & lines] input
         position 50
         cnt 0]
    (if-not line
      cnt
      (let [direction (case (subs line 0 1)
                        "L" -
                        "R" +)
            ticks (Integer/parseInt (subs line 1))
            next-position (mod (direction position ticks) 100)
            next-cnt (if (= next-position 0)
                       (inc cnt)
                       cnt)]
        (recur lines next-position next-cnt)))))

(defn pt2 [input]
  (loop [[line & lines] input
         position 50
         cnt 0]
    (if-not line
      cnt
      (let [direction (case (subs line 0 1)
                        "L" -
                        "R" +)
            ticks (Integer/parseInt (subs line 1))
            next-position (mod (direction position ticks) 100)
            next-cnt (loop [position' position
                            ticks' ticks
                            cnt' cnt]
                       (let [next-p (mod (direction position' 1) 100)]
                         (if (= ticks' 0)
                           cnt'
                           (recur next-p
                                  (dec ticks')
                                  (if (zero? next-p)
                                    (inc cnt')
                                    cnt')))))]
        (recur lines next-position next-cnt)))))

(comment
  (time (pt1 (std/read-input 25 1)))
  (time (pt2 (std/read-input 25 1)))
  :-)
