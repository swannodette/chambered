(ns chambered.core
  (:refer-clojure :exclude [reset!])
  (:use-macros [chambered.macros :only [forloop reset!]])
  (:require [chambered.util]))

;; =============================================================================
;; Declarations

(def timer nil)
(def w (* 212 2))
(def h (* 210 2))
(def ctx (.getContext (.getElementById js/document "game") "2d"))
(def pixels (.createImageData ctx w h))
(def blockmap (make-array (* 64 64 64)))

(defn gen-texmap []
  (let [arr (array)]
    (loop [i 0]
      (if (< i (* 16 16 3 16))
        (do
          (.push arr 0)
          (recur (inc i)))
        arr))))

(def texmap (gen-texmap))

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

(declare clock)

;; Notch's clever texture generator, we use Boxes since we can't bash
;; on locals, data looks completely different! - David
(defn init []
  ;; Generate the textures
  (let [color (Box. nil)
        br    (Box. nil)
        brr   (Box. nil)]
    (forloop [(i 1) (< i 16) (inc i)]
      (reset! br (- 255 (random-int 96)))
      (forloop [(y 0) (< y (* 16 3)) (inc y)]
        (forloop [(x 0) (< x 16) (inc x)]
          (reset! color 0x966C4A)
          ;; Stone
          (when (== i 4)
            (reset! color 0x7F7F7F))
          (when (or (not (== i 4)) (zero? (random-int 3)))
            (reset! br (- 255 (random-int 96))))
          ;; Grass
          (when (== i 1)
            (cond
              (< y (+ (bitop x) 18)) (reset! color 0x6AAA40)
              (< y (+ (bitop x) 19)) (reset! br (/ (* (.-val br) 2) 3))))
          ;; Tree trunk
          (when (== i 7)
            (reset! color 0x675231)
            (if (and (in? x 0 15) (or (in? y 0 15) (in? y 32 47)))
              (do
                (reset! color 0xBC9862)
                (let [xd (Box. (- x 7))
                       yd (Box. (- (bit-and y 15) 7))]
                  (when (neg? (.-val xd))
                    (reset! xd (- 1 (.-val xd))))
                  (when (neg? (.-val yd))
                    (reset! yd (- 1 (.-val yd))))
                  (when (> (.-val yd) (.-val xd))
                    (reset! xd (.-val yd)))
                  (reset! br (- 196 (rand-int 32) (* (js-mod (.-val xd) 3) 32)))))
              (if (zero? (rand-int 2))
                (reset! br (/ (* (.-val br) (- 150 (* (bit-and x 1) 100))) 100)))))
          ;; Brick
          (when (== i 5)
            (reset! color 0xB53A15)
            (when (or (zero? (js-mod (+ x (* (bit-shift-right y 2) 4)) 8))
                      (zero? (js-mod y 4)))
              (reset! color 0xBCAFA5)))
          ;; Water
          (when (== i 9)
            (reset! color 0x4040FF))
          (reset! brr (.-val br))
          (when (>= y 32)
            (reset! brr (/ (.-val brr) 2)))
          ;; Leaves
          (when (== i 8)
            (reset! color 0x50D937)
            (if (zero? (rand-int 2))
              (reset! color 0)
              (reset! brr 255)))
          (let [c   (.-val color)
                brr (.-val brr)]
            (aset texmap (+ x (* y 16) (* i 256 3))
              (bit-or
                (color-int c brr 16)
                (color-int c brr 8)
                (color-int c brr)))))))

    ;; generate the block map
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

    (forloop [(i 0) (< i (* w h)) (inc i)]
      (aset (.-data pixels) (+ (* i 4) 3) 255))
    
    (set! timer (js/setInterval clock (/ 1000 100)))))

(declare render-minecraft)

(defn clock []
  (let [s (js/Date.)]
    (render-minecraft)
    ;;(.log js/console (- (js/Date.) s))
    (.putImageData ctx pixels 0 0)
    ;;(js/clearInterval timer)
    ))

(defn date-seed []
  (/ (js-mod (.now js/Date) 10000) 10000))

(defn render-minecraft []
  (let [twopi  (* js/Math.PI 2)
        halfpi (/ js/Math.PI 2)
        ds     (date-seed)
        xrot (+ (* (.sin js/Math (* ds twopi)) 0.4) halfpi)
        yrot (* (.cos js/Math (* ds twopi)) 0.4)
        ycos (.cos js/Math yrot)
        ysin (.sin js/Math yrot)
        xcos (.cos js/Math xrot)
        xsin (.sin js/Math xrot)
        ox   (+ 32.5 (* ds 64))
        oy   32.5
        oz   32.5
        col     (make-array 1)
        br      (make-array 1)
        ddist   (make-array 1)
        dist    (make-array 1)
        closest (make-array 1)]
    (forloop [(x 0) (< x w) (inc x)]
      (let [xd''' (/ (- x (/ w 2)) h)]
        (forloop [(y 0) (< y h) (inc y)]
          (let [yd''  (/ (- y (/ h 2)) h)
                zd''  1
                zd''' (+ (* zd'' ycos) (* yd'' ysin))
                yd'   (- (* yd'' ycos) (* zd'' ysin))
                xd'   (+ (* xd''' xcos) (* zd''' xsin))
                zd'   (- (* zd''' xcos) (* xd''' xsin))]
            (aset col 0 0)
            (aset br 0 255)
            (aset ddist 0 0)
            (aset closest 0 32)
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
                (aset dist 0 (* ll initial))
                (loop [xp xp yp yp zp zp]
                  (if (< (aget dist 0) (aget closest 0))
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
                            (aset col 0 cc)
                            (aset ddist 0 (- 255 (bit-or (* (/ (aget dist 0) 32) 255) 0)))
                            (aset br 0 (/ (* 255 (- 255 (* mexp 50))) 255))
                            (aset closest 0 (aget dist 0)))))
                      (aset dist 0 (+ (aget dist 0) ll))
                      (recur (+ xp xd) (+ yp yd) (+ zp zd)))))))
            (let [br    (aget br 0)
                  ddist (aget ddist 0)
                  col   (aget col 0)
                  r     (/ (* (bit-and (bit-shift-right col 16) 0xFF) br ddist) (* 255 255)) 
                  g     (/ (* (bit-and (bit-shift-right col 8) 0xFF) br ddist) (* 255 255))
                  b     (/ (* (bit-and col 0xFF) br ddist) (* 255 255))
                  data  (.-data pixels)
                  p     (* (+ x (* y w)) 4)]
              (aset data (+ p 0) r)
              (aset data (+ p 1) g)
              (aset data (+ p 2) b))))))))

(init)

