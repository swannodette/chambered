(ns chambered.core
  (:refer-clojure :exclude [reset!])
  (:use-macros [chambered.macros :only [forloop]]))

(extend-type Box
  IDeref
  (-deref [this]
    (.-val this)))

(defn reset! [box value]
  (set! (.-val box) value))

(def w (* 212 2))
(def h (* 210 2))

(def ctx (.getContext (.getElementById js/document "game") "2d"))
(def pixels (.createImageData ctx w h))

(def blockmap (make-array (* 64 64 64)))
;; still now sure about this layout
(def texmap (make-array (* 16 16 3 16)))

(defn random [n]
  (* (.random js/Math) n))

(defn random-int [n]
  (bit-or (random n) 0))

(defn ^boolean in? [n lb ub]
  (and (> n lb) (< n ub)))

(defn bitop [x]
  (bit-and (bit-shift-right (+ (* x x 3) (* x 81)) 2) 3))

(defn color-int
  ([n brr]
    (color-int n brr 0))
  ([n brr shift]
    (-> n
      (bit-shift-right shift)
      (bit-and 0xff) (* brr) (/ 255)
      (bit-shift-left shift))))

;; for figuring out what Notch was thinking

(defn render-into [src dest ox oy w h]
  (forloop [(x ox) (< x (+ ox w)) (inc x)]
    (forloop [(y oy) (< y (+ oy h) (inc y))]
      )))

(declare clock)

(defn init []
  (let [color (Box. nil)
        br    (Box. nil)
        brr   (Box. nil)]
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
              (reset! br (/ (* @br 2) 3))))
          (when (== i 7)
            (reset! color 0x675231)
            (when (and (in? x 0 15) (or (in? y 0 15) (in? y 32 47)))
              (reset! color 0xBC9862)
              (let [xd (Box. (- x 7))
                    yd (Box. (- (bit-and y 15) 7))]
                (when (neg? xd)
                  (reset! xd (- 1 @xd)))
                (when (neg? yd)
                  (reset! yd (- 1 @yd)))
                (when (> yd xd)
                  (reset! xd @yd))
                (reset! br (- 196 (rand-int 32) (* (mod xd 3) 32)))))
            (when (zero? (rand-int 2))
              (reset! br (/ (* @br (- 150 (* (bit-and x 1) 100))) 100))))
          (when (== i 5)
            (reset! color 0xB53A15)
            (when (or (zero? (+ x (mod (bit-shift-right y 2) 8)))
                      (zero? (mod y 4)))
              (reset! color 0xBCAFA5)))
          (when (== i 9)
            (reset! color 0x4040FF))
          (reset! brr @br)
          (when (>= y 32)
            (reset! brr (/ @brr 2)))
          (let [c @color]
            (aset texmap (+ x (* y 16) (* i 256 3))
              (bit-or (color-int c brr 16)
                (color-int c brr 8) (color-int brr c)))))))

    #(js/setInterval clock (/ 1000 60))

    #_(let [ctx ]
      (forloop [(x 0) (< x 64) (inc x)]
        (forloop [(y 0) (< x 64) (inc x)]
          (forloop [(z 0) (< x 64) (inc x)]
            (let [i (bit-or (bit-shift-left z 12)
                            (bit-shift-left y 6)
                            x)
                  yd (* (- y 32.5) 0.4)
                  zd (* (- z 32.5) 0.4)]
              (aset map i (rand-int 16))
              (when (> (.random js/Math)
                       (- (.sqrt js/Math
                            (.sqrt js/Math
                              (+ (* yd yd) (* zd zd))
                          0.8))))
                (aset map i 0))))))))
  )

(declare render-minecraft)

(defn clock []
  (render-minecraft)
  (.putImageData ctx texmap 0 0))

(defn render-minecraft []
  )

(init)
(clock)
