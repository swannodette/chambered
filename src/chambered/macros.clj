(ns chambered.macros
  (:refer-clojure :exclude [reset!]))

(defmacro forloop [[init test step] & body]
  `(loop [~@init]
     (when ~test
       ~@body
       (recur ~step))))

(defmacro local
  ([]
    `(make-array 1))
  ([x]
    `(cljs.core/array ~x)))

(defmacro >> [x v]
  `(aset ~x 0 ~v))

(defmacro << [x]
  `(aget ~x 0))
