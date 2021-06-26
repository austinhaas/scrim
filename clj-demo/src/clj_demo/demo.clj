
;; This walkthrough assumes that you're following the guide in the README and you have already
;; installed Scrim and started a Java process with a socket server.

;; The code is meant to be evaluated one sexp at a time.

;; View all Scrim keybindings:
;; C-h f scrim-minor-mode RET

;; Connect Scrim
;; M-x scrim-connect RET localhost RET 5555 RET

;; You should see a REPL. Hopefully, this buffer is still visible.

;; Move point (i.e., the cursor) just after the namespace declaration a few
;; lines below.

;; Evaluate it with:

;; C-c C-e - evaluate previous sexp

(ns clj-demo.demo)

;; Move point anywhere inside the following function definition.

;; C-c C-c - evaluate toplevel sexp around point (or previous sexp if point isn't in a sexp)

(defn add-1
  "Adds 1 to x."
  [x]
  (inc x))

;; If the REPL is hidden, C-c C-z will show it.

;; Move point inside the following sexp.

;; C-c C-r C-d - show documentation

;; C-c C-S-a - show arglists

(add-1 1)

;; Move point just after the + in the following sexp.

;; C-c C-r C-s - show source

(+ 1 2)

;; Move point just after the following sexp.

;; C-c C-m - macroexpand

(time (print "hello"))

;; C-c C-p C-p - pretty-print last result

;;; More useful commands

;; C-c r - require namespace

;; C-c C-n - switch to namespace

;; C-c C-r C-S-d - show public vars in namespace

;; C-c C-q - disconnect from REPL

;; Note that some commands take an optional prefix argument (C-u), which triggers a prompt for
;; input, rather than deriving it from the position of point in a buffer.
