(ns clj-demo.other)

(defn foo
  ([] 'ok)
  ([x] x))

(defmulti bar odd?)

(defmethod bar true [_] "odd")

(defmethod bar false [_] "even")
