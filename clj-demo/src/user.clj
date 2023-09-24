(ns user
  (:require
   [cljs.repl :as repl]
   [cljs.repl.browser :as browser]
   [cljs.repl.node :as node]
   [clojure.core.server :refer [prepl]]
   [clojure.edn :as edn]
   [clojure.pprint :refer [cl-format]]))

(def compile-opts-files ["config/compile-opts-dev.edn"])

(defn browser-repl []
  (let [env          (browser/repl-env :launch-browser false)
        compile-opts (->> (map (comp edn/read-string slurp) compile-opts-files)
                          (reduce merge))]
    (repl/repl* env compile-opts)))

(defn node-repl []
  (let [env          (node/repl-env :launch-browser false)
        compile-opts (->> (map (comp edn/read-string slurp) compile-opts-files)
                          (reduce merge))]
    (repl/repl* env compile-opts)))

(defn- print-map-as-alist [m]
  (print "(")
  (doseq [[k v] m]
    (print "(")
    (pr k)
    (print " . ")
    (pr v)
    (print ")"))
  (print ")")
  (newline)
  (flush))

(defn my-io-prepl
  "prepl bound to *in* and *out*, suitable for use with e.g. server/repl (socket-repl)."
  []
  (let [out  *out*
        lock (Object.)]
    (prepl *in*
           (fn [m]
             (binding [*out* out, *flush-on-newline* true, *print-readably* true]
               (let [m (assoc m :*out* (str *out*))]
                (locking lock
                  (print-map-as-alist
                       (if (#{:ret :tap} (:tag m))
                         (assoc m :val (pr-str (:val m)))
                         m)))))))))
