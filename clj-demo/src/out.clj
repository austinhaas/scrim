(ns out
  "Change *out*, *err*, System/out and System/err to print on local repl.

  https://github.com/clojure-emacs/cider/issues/1588

  https://github.com/clojure-emacs/cider-nrepl/blob/65e6a1115e55405e108e96f63c7098ed586f0027/src/cider/nrepl/middleware/out.clj

  https://groups.google.com/g/clojure/c/t3Pp8l9Pe4A/m/W4x9MsjPBAAJ"
  (:import [java.io PrintWriter Writer PrintStream OutputStream]))

(defn print-stream
  "Returns a PrintStream suitable for binding as java.lang.System/out
  or java.lang.System/err. All operations are forwarded to all output
  bindings in the sessions of messages in addition to the server's
  usual PrintWriter (saved in `original-out` or `original-err`).
  type is either :out or :err."
  [type]
  (let [printer (case type
                  :out '*out*
                  :err '*err*)]
    (PrintStream. (proxy [OutputStream] []
                    (close [] (.flush ^OutputStream this))
                    (write
                      ([bytes]
                       (.write @(resolve printer) (String. bytes)))
                      ([bytes ^Integer off ^Integer len]
                       (let [byte-range (byte-array
                                         (take len (drop off bytes)))]
                        (.write @(resolve printer) (String. byte-range)))))
                    (flush []
                      (.flush @(resolve printer))))
                  true)))

(defn init []

  (alter-var-root #'*out* (fn [v]
                            (defonce original-out v)
                            *out*))
  (alter-var-root #'*err* (fn [v]
                            (defonce original-err v)
                            *err*))

  (System/setOut (print-stream :out))
  (System/setErr (print-stream :err)))

(comment

  (+ 1 2)

  (do (print 'ok)
      (flush))

  (range 100)

  (/ 1 0)

  (throw (ex-info "my ex-info" {}))

  (try
    (throw (Exception. "My first exception"))
    (catch Exception e
      (println "My first message")
      (.printStackTrace e)
      e))

  (let [out *out*]
    (.start
     (Thread.
      (fn []
        (binding [*out* out]
          (println "My second message.")
          (throw (ex-info "My third exception" {})))))))

)
