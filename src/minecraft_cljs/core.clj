(ns minecraft-cljs.core
  (:use-macros [minecraft-cljs.macros :only [forloop]]))

(def w (* 212 2))
(def h (* 210 2))

(def map (make-array (* 64 64 64)))
(def texmap (make-array (* 16 16 3 16)))

(defn random [n]
  (* (.random js/Math) n))

(defn random-int [n]
  (bit-or (random n) 0))

(defn in? [n lb ub]
  (and (> n lb) (< n ub)))

(defn bitop [x]
  (bit-and (bit-shift-right (+ (* x x 3) (* x 81)) 2) 3))

(defn init []
  (let [color (atom nil)
        br    (atom nil)]
    (forloop [(i 1) (< i 16) (inc i)]
      (reset! color (- 255 (random-int 96)))
      (forloop [(y 0) (< y (* 16 3)) (inc y)]
        (forloop [(x 0) (< x 16) (inc x)]
          (when (== i 4)
            (reset! color 0x7F7F7F))
          (when (or (not (== i 4)) (zero? (random-int 3)))
            (reset! br (- 255 (random-int 96))))
          (when (== i 1)
            (when (< y (+ (bitop x) 18))
              (reset! color 0x6AAA40))
            (when (< y (+ (bitop x) 19))
              (swap! br (fn [n] (/ (* n 2) 3)))))
          (when (== i 7)
            (reset! color 0x675231)
            (when (and (in? x 0 15) (or (in? y 0 15) (in? y 32 47)))
              (reset! color 0xBC9862)
              (let [xd (atom (- x 7))
                    yd (atom (- (bit-and y 15) 7))]
                )))))))))
