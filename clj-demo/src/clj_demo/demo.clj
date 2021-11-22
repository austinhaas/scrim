;; This walkthrough assumes that you're following the guide in the README and you have already
;; installed Scrim.

;; The code is meant to be evaluated one sexp at a time.

;; If you started a socket server,
;; Connect Scrim:
;; M-x scrim-connect RET localhost RET 5555 RET

;; If you did not start a socket server,
;; Start Scrim:
;; M-x scrim RET clojure RET

;; You should see a REPL. Hopefully, this buffer is still visible.

;; View all Scrim keybindings:
;; C-h f scrim-minor-mode RET

;; Show the REPL again:
;; C-c C-z

;; If the REPL is currently visible, C-C C-z tries to return to whatever buffer
;; was last visible.

;; Move point (i.e., the cursor) just after the namespace declaration a few
;; lines below.

;; Evaluate it with:

;; C-c C-e - evaluate previous sexp

(ns clj-demo.demo
  (:require
   [clj-demo.other :as other]
   [clojure.string :refer [join]]))

;; Move point anywhere inside the following function definition.

;; C-c C-c - evaluate top-level sexp around point (or previous sexp if point isn't in a sexp)

(defn add-1
  "Adds 1 to x."
  [x]
  (inc x))

;; Move point inside the following sexp.

;; C-c C-d d <RET> - show documentation

;; C-c C-a - show arglists

(add-1 1)

;; Move point just after the + in the following sexp.

;; C-c C-d s - show source

(+ 1 2)

;; Move point just after the following sexp.

;; C-c C-m m - macroexpand

(time (println "hello"))

;; C-c p - pretty-print last result

;;; More useful commands

;; C-c C-r - require namespace

;; C-c C-n - switch to namespace

;; C-c C-M-d - show public vars in namespace

;; C-c C-q - disconnect from REPL

;; Note that some commands take an optional prefix argument (C-u), which triggers a prompt for
;; input, rather than deriving it from the position of point in a buffer.

(other/foo)

(clj-demo.other/foo)

other/bar
