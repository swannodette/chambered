(ns chambered.macros
  (:refer-clojure :exclude [reset!]))

(defmacro forloop [[init test step] & body]
  `(loop [~@init]
     (when ~test
       ~@body
       (recur ~step))))

(defmacro reset! [x v]
  `(set! (.-val ~x) ~v))
