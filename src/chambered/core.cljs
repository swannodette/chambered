(ns chambered.core
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

;; =============================================================================
;; Declarations

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
              (< y (+ (bitop x) 19)) (reset! br (/ (* @br 2) 3))))
          ;; Tree trunk
          (when (== i 7)
            (reset! color 0x675231)
            (if (and (in? x 0 15) (or (in? y 0 15) (in? y 32 47)))
              (do
                (reset! color 0xBC9862)
                (let [xd (Box. (- x 7))
                       yd (Box. (- (bit-and y 15) 7))]
                  (when (neg? @xd)
                    (reset! xd (- 1 @xd)))
                  (when (neg? @yd)
                    (reset! yd (- 1 @yd)))
                  (when (> @yd @xd)
                    (reset! xd @yd))
                  (reset! br (- 196 (rand-int 32) (* (mod @xd 3) 32)))))
              (if (zero? (rand-int 2))
                (reset! br (/ (* @br (- 150 (* (bit-and x 1) 100))) 100)))))
          ;; Brick
          (when (== i 5)
            (reset! color 0xB53A15)
            (when (or (zero? (mod (+ x (* (bit-shift-right y 2) 4)) 8))
                      (zero? (mod y 4)))
              (reset! color 0xBCAFA5)))
          ;; Water
          (when (== i 9)
            (reset! color 0x4040FF))
          (reset! brr @br)
          (when (>= y 32)
            (reset! brr (/ @brr 2)))
          ;; Leaves
          (when (== i 8)
            (reset! color 0x50D937)
            (if (zero? (rand-int 2))
              (reset! color 0)
              (reset! brr 255)))
          (let [c   @color
                brr @brr]
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

    ;; set the alpha channel on all pixels
    
    #(js/setInterval clock (/ 1000 60))))

(declare render-minecraft)

(defn clock []
  ;;(render-minecraft)
  ;;(copy-texmap-into-pixels texmap pixels)
  (.putImageData ctx pixels 0 0)
  (.log js/console "done!")
  )

(def f (Box. 0))

(defn render-color [c br ddist shift]
  (/ (* (bit-and (bit-shift-right c shift) 0xFF) br ddist) (* 255 255)))

(defn render-minecraft []
  (let [xrot (* (.sin js/Math
                  (* (/ (mod (.now js/Date) 10000) 10000) (.PI js/Math) 2)) 0.4)
        yrot (* (.cos js/Math
                  (* (/ (mod (.now js/Date) 10000) 10000) (.PI js/Math) 2)) 0.4)
        ycos (.cos js/Math yrot)
        ysin (.sin js/Math yrot)
        xcos (.cos js/Math xrot)
        xsin (.sin js/Math xrot)
        ox   (+ 32.5 (* (/ (mod (.now js/Date 10000) 10000)) 64))
        oy   32.5
        oz   32.5]
    (reset! f (inc @f))
    (forloop [(x 0) (< x w) (inc x)]
      (let [xd''' (/ (/ (- x w) 2) h)]
        (forloop [(y 0) (< y h) (inc y)]
          (let [yd''  (/ (/ (- y h) 2) h)
                zd''  1
                zd''' (+ (* zd'' ycos) (* yd'' ysin))
                yd'   (- (* yd'' ycos) (* zd'' ysin))
                xd'   (+ (* xd''' xcos) (* zd''' xsin))
                zd'   (- (* zd''' xcos) (* xd''' xsin))
                col   (Box. 0)
                br    (Box. 255)
                ddist (Box. 0)
                closest (Box. 32)]
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
                              (== d 0) (- ox (bit-and ox 0))
                              (== d 1) (- oy (bit-and oy 0))
                              (== d 2) (- oz (bit-and oz 0)))
                    dist (* ll initial)
                    xp (Box. (cond-> (+ ox (* xd initial))
                               (and (== d 0) (neg? dim-length)) dec))
                    yp (Box. (cond-> (+ oy (* yd initial))
                               (and (== d 1) (neg? dim-length)) dec))
                    zp (Box. (cond-> (+ oz (* zd initial))
                               (and (== d 2) (neg? dim-length)) dec))]
                (while (< @dist @closest)
                  (let [tex (aget blockmap [(bit-or (bit-shift-left (bit-and @zp 63) 12)
                                                    (bit-shift-left (bit-and @yp 63) 6)
                                                    (bit-and @xp 63))])]
                    (when (pos? tex)
                      (let [u (if (== d 1)
                                (bit-and (* @xp 16) 15)
                                (bit-and (* (+ @xp @zp) 16) 15))
                            v (if (== d 1)
                                (cond-> (bit-and (* @zp 16) 15)
                                  (neg? yd) (+ 32))
                                (+ (bit-and (* @yp 16) 15) 16))
                            cc (aget texmap (+ u (* v 16) (* tex 256 3)))]
                        (when (pos? cc)
                          (reset! col cc)
                          (reset! ddist (- 255 (bit-or (* (/ dist 32) 155) 0)))
                          (reset! br (* 255 (- 255 (/ (* (mod (+ d 2) 3) 50) 255))))
                          (reset! closest dist))))
                    (reset! xp (+ @xp xd))
                    (reset! yp (+ @yp yd))
                    (reset! zp (+ @zp zd))
                    (reset! dist (+ @dist ll))))))
            (let [br    @br
                  ddist @ddist
                  col   @col
                  r     (render-color col br ddist 16)
                  g     (render-color col br ddist 8)
                  b     (render-color col br ddist 0)
                  data  (.-data pixels)
                  p     (+ (* (+ x (* y w)) 4) 0)]
              (aset data (+ p 0) r)
              (aset data (+ p 1) g)
              (aset data (+ p 2) b))))))))

(init)
(clock)
