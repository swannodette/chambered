(ns chambered.core
  (:use-macros [chambered.macros :only [forloop << >> local]])
  (:require [chambered.util]))

;; =============================================================================
;; Utilities

(defn random [n]
  (* (.random js/Math) n))

(defn random-int [n]
  (bit-or (random n) 0))

(defn ^boolean in? [n lb ub]
  (and (> n lb) (< n ub)))

(defn bitop [x]
  (bit-and (bit-shift-right (+ (* x x 3) (* x 81)) 2) 3))

(defn color-int
  ([color brr]
    (color-int color brr 0))
  ([color brr shift]
    (-> color
      (bit-shift-right shift)
      (bit-and 0xFF) (* brr) (/ 255)
      (bit-shift-left shift))))

;; =============================================================================
;; Procedural Texture & Block generation

(defn gen-texmap []
  (let [texmap (make-array (* 16 16 3 16))
        color  (local)
        br     (local)
        brr    (local)]
    (forloop [(i 1) (< i 16) (inc i)]
      (>> br (- 255 (random-int 96)))
      (forloop [(y 0) (< y (* 16 3)) (inc y)]
        (forloop [(x 0) (< x 16) (inc x)]
          (>> color 0x966C4A)
          (when (== i 4)
            (>> color 0x7F7F7F))
          (when (or (not (== i 4)) (zero? (random-int 3)))
            (>> br (- 255 (random-int 96))))
          (when (== i 1)
            (cond
              (< y (+ (bitop x) 18)) (>> color 0x6AAA40)
              (< y (+ (bitop x) 19)) (>> br (/ (* (<< br) 2) 3))))
          (when (== i 7)
            (>> color 0x675231)
            (if (and (in? x 0 15) (or (in? y 0 15) (in? y 32 47)))
              (do
                (>> color 0xBC9862)
                (let [xd (local (- x 7))
                      yd (local (- (bit-and y 15) 7))]
                  (when (neg? (<< xd))
                    (>> xd (- 1 (<< xd))))
                  (when (neg? (<< yd))
                    (>> yd (- 1 (<< yd))))
                  (when (> (<< yd) (<< xd))
                    (>> xd (<< yd)))
                  (>> br (- 196 (rand-int 32) (* (js-mod (<< xd) 3) 32)))))
              (if (zero? (rand-int 2))
                (>> br (/ (* (<< br) (- 150 (* (bit-and x 1) 100))) 100)))))
          (when (== i 5)
            (>> color 0xB53A15)
            (when (or (zero? (js-mod (+ x (* (bit-shift-right y 2) 4)) 8))
                    (zero? (js-mod y 4)))
              (>> color 0xBCAFA5)))
          (when (== i 9)
            (>> color 0x4040FF))
          (>> brr (<< br))
          (when (>= y 32)
            (>> brr (/ (<< brr) 2)))
          (when (== i 8)
            (>> color 0x50D937)
            (if (zero? (rand-int 2))
              (>> color 0)
              (>> brr 255)))
          (let [c   (<< color)
                brr (<< brr)]
            (aset texmap (+ x (* y 16) (* i 256 3))
              (bit-or
                (color-int c brr 16)
                (color-int c brr 8)
                (color-int c brr)))))))
    texmap))

(defn gen-blockmap []
  (let [blockmap (make-array (* 64 64 64))]
    (forloop [(x 0) (< x 64) (inc x)]
      (forloop [(y 0) (< y 64) (inc y)]
        (forloop [(z 0) (< z 64) (inc z)]
          (let [i (bit-or (bit-shift-left z 12) (bit-shift-left y 6) x)
                yd (* (- y 32.5) 0.4)
                zd (* (- z 32.5) 0.4)]
            (aset blockmap i (rand-int 16))
            (when (> (.random js/Math)
                     (- (.sqrt js/Math (.sqrt js/Math (+ (* yd yd) (* zd zd)))) 0.8))
              (aset blockmap i 0))))))
    blockmap))

;; =============================================================================
;; Declarations

(def w (* 212 2))
(def h (* 120 2))
(def twopi (* js/Math.PI 2))
(def halfpi (/ js/Math.PI 2))
(def ctx (.getContext (.getElementById js/document "game") "2d"))
(def blockmap (gen-blockmap))
(def texmap (gen-texmap))

(declare render-minecraft!)

(defn clock []
  (render-minecraft! ctx))

(defn init []
  (js/setInterval clock (/ 1000 100)))

(defn date-seed []
  (/ (js-mod (.now js/Date) 10000) 10000))

(defn render-minecraft! [ctx]
  (let [pixels (.createImageData ctx w h)
        ds   (date-seed)
        xrot (+ (* (.sin js/Math (* ds twopi)) 0.4) halfpi)
        yrot (* (.cos js/Math (* ds twopi)) 0.4)
        ycos (.cos js/Math yrot)
        ysin (.sin js/Math yrot)
        xcos (.cos js/Math xrot)
        xsin (.sin js/Math xrot)
        ox   (+ 32.5 (* ds 64))
        oy   32.5
        oz   32.5
        col     (local)
        br      (local)
        ddist   (local)
        dist    (local)
        closest (local)]
    (forloop [(i 0) (< i (* w h)) (inc i)]
      (aset (.-data pixels) (+ (* i 4) 3) 255))
    (forloop [(x 0) (< x w) (inc x)]
      (let [xd''' (/ (- x (/ w 2)) h)]
        (forloop [(y 0) (< y h) (inc y)]
          (let [yd''  (/ (- y (/ h 2)) h)
                zd''  1
                zd''' (+ (* zd'' ycos) (* yd'' ysin))
                yd'   (- (* yd'' ycos) (* zd'' ysin))
                xd'   (+ (* xd''' xcos) (* zd''' xsin))
                zd'   (- (* zd''' xcos) (* xd''' xsin))]
            (>> col 0)
            (>> br 255)
            (>> ddist 0)
            (>> closest 32)
            (forloop [(d 0) (< d 3) (inc d)]
              (let [dim-length (cond
                                 (== d 0) xd'
                                 (== d 1) yd'
                                 (== d 2) zd')
                    ll (/ 1 (if (neg? dim-length) (- dim-length) dim-length))
                    xd (* xd' ll)
                    yd (* yd' ll)
                    zd (* zd' ll)
                    initial (cond
                              (== d 0) (- ox (bit-or ox 0))
                              (== d 1) (- oy (bit-or oy 0))
                              (== d 2) (- oz (bit-or oz 0)))
                    initial (if (pos? dim-length) (- 1 initial) initial)
                    xp (+ ox (* xd initial))
                    xp (if (and (== d 0) (neg? dim-length)) (dec xp) xp)
                    yp (+ oy (* yd initial))
                    yp (if (and (== d 1) (neg? dim-length)) (dec yp) yp)
                    zp (+ oz (* zd initial))
                    zp (if (and (== d 2) (neg? dim-length)) (dec zp) zp)]
                (>> dist (* ll initial))
                (loop [xp xp yp yp zp zp]
                  (if (< (<< dist) (<< closest))
                    (let [tex (aget blockmap
                                (bit-or
                                  (bit-shift-left (bit-and zp 63) 12)
                                  (bit-shift-left (bit-and yp 63) 6)
                                  (bit-and xp 63)))]
                      (when (pos? tex)
                        (let [u (if (== d 1)
                                  (bit-and (* xp 16) 15)
                                  (bit-and (* (+ xp zp) 16) 15))
                              v (if (== d 1)
                                  (cond-> (bit-and (* zp 16) 15)
                                    (neg? yd) (+ 32))
                                  (+ (bit-and (* yp 16) 15) 16))
                              cc (aget texmap (+ u (* v 16) (* tex 256 3)))
                              mexp (js-mod (+ d 2) 3)]
                          (when (pos? cc)
                            (>> col cc)
                            (>> ddist (- 255 (bit-or (* (/ (<< dist) 32) 255) 0)))
                            (>> br (/ (* 255 (- 255 (* mexp 50))) 255))
                            (>> closest (<< dist)))))
                      (>> dist (+ (<< dist) ll))
                      (recur (+ xp xd) (+ yp yd) (+ zp zd)))))))
            (let [br    (<< br)
                  ddist (<< ddist)
                  col   (<< col)
                  r     (/ (* (bit-and (bit-shift-right col 16) 0xFF) br ddist) (* 255 255)) 
                  g     (/ (* (bit-and (bit-shift-right col 8) 0xFF) br ddist) (* 255 255))
                  b     (/ (* (bit-and col 0xFF) br ddist) (* 255 255))
                  data  (.-data pixels)
                  p     (* (+ x (* y w)) 4)]
              (aset data (+ p 0) r)
              (aset data (+ p 1) g)
              (aset data (+ p 2) b))))))
    (.putImageData ctx pixels 0 0)))

(init)

