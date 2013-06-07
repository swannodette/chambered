(ns chambered.util
  (:refer-clojure :exclude [reset!])
  (:use-macros [chambered.macros :only [forloop]]))

;; =============================================================================
;; Atoms have a lot dependencies so we use and extend box instead

(extend-type Box
  IDeref
  (-deref [this]
    (.-val this)))

(defn reset! [box value]
  (set! (.-val box) value))

;; for figuring out what Notch was thinking

(defn red [n] (bit-and (bit-shift-right n 16) 0xFF))
(defn green [n] (bit-and (bit-shift-right n 8) 0xFF))
(defn blue [n] (bit-and n 0xFF))

(defn set-pixel [pixels x y c]
  (let [width  (.-width pixels)
        height (.-height pixels)
        data   (.-data pixels)
        pi     (* (+ x (* y width)) 4)]
    (aset data (+ pi 0) (red c))
    (aset data (+ pi 1) (green c))
    (aset data (+ pi 2) (blue c))
    (aset data (+ pi 3) 0xFF)))

;; for debugging textures
(defn copy-texmap-into-pixels [texmap pixels]
  (let [counter (Box. 0)]
    (forloop [(i 0) (< i 16) (inc i)]
      (forloop [(y 0) (< y (* 16 3)) (inc y)]
        (forloop [(x 0) (< x 16) (inc x)]
          (let [c (aget texmap @counter)]
            (set-pixel pixels
              (+ x (* (.floor js/Math (/ i 5)) 16))
              (+ y (mod (* i 16 3) (* 16 3 5))) c)
            (reset! counter (inc @counter))))))))
