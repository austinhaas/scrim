;;; scrim.el --- Simple Clojure REPL Interaction Mode

;; Copyright © 2021 Austin Haas
;;
;; Author: Austin Haas <austin@pettomato.com>
;; URL: http://github.com/austinhaas/scrim
;; Version: 0.0.6-SNAPSHOT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; See README.md.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ansi-color)
(require 'arc-mode)
(require 'cl-lib)
(require 'clojure-mode)
(require 'comint)
(require 'subr-x)
(require 'thingatpt)
(require 'xref)


(defconst scrim-version "0.0.6-SNAPSHOT"
  "The current version of `Scrim'.")

;;;; Utility

;; TODO: Replace this with completing-read.
(defun scrim--prompt (prompt default)
  "Prompt user for input. If default is not nil, it will be
included in the prompt, and returned as the value if the user
enters a blank string."
  (let* ((prompt (if default
                     (format "%s (default %s): " prompt default)
                   (format "%s: " prompt)))
         (ans    (read-string prompt)))
    (if (string-blank-p ans)
        default
      ans)))

;;;; Functions to extract expressions from Clojure buffers

(defun scrim-symbol-at-point ()
  (thing-at-point 'symbol t))

(defun scrim-previous-sexp ()
  "Returns the sexp before point."
  (save-excursion
    (let ((start (point)))
      (backward-sexp)
      (let ((beginning (point)))
        (forward-sexp)
        (let ((end (point)))
          (when (<= end start)
            (buffer-substring-no-properties beginning end)))))))

(defun scrim--function-invocation-at-pos-p (pos)
  (save-excursion
    (goto-char pos)
    (if (= (point-min) (point))
        (looking-at-p "(")
      (backward-char)
      (looking-at-p "[^']("))))

(defun scrim--current-function-invocation-pos ()
    "Returns the character address of the innermost containing
function invocation; nil if none."
  (let ((ps (reverse (nth 9 (syntax-ppss)))))
    (while (and (consp ps)
                (not (scrim--function-invocation-at-pos-p (car ps))))
      (setq ps (cdr ps)))
    (car ps)))

(defun scrim-backward-function-invocation ()
  "Move point to the start of the function invocation containing
point."
  (let ((pos (scrim--current-function-invocation-pos)))
    (if pos
        (goto-char pos)
      (user-error "Not in a function invocation."))))

(defun scrim-current-function-symbol ()
  "Returns the symbol in function position in the innermost sexp
around point."
  (condition-case nil
      (save-excursion
        (scrim-backward-function-invocation)
        (forward-thing 'symbol)
        (thing-at-point 'symbol t))
    (error nil)))

(defun scrim-outer-around-sexp ()
  "Returns the outer sexp around point."
  (save-excursion
    (let ((start (point)))
      (beginning-of-defun)
      (forward-sexp)
      (let ((end (point)))
        (backward-sexp)
        (let ((beginning (point)))
          (when (< beginning start end)
            (buffer-substring-no-properties beginning end)))))))

(defun scrim-outer-around-or-previous-sexp ()
  "Returns the outer sexp around point, if point is inside
  a sexp, otherwise returns the sexp before point."
  (or (scrim-outer-around-sexp)
      (scrim-previous-sexp)))

(defun scrim-sexps-in-region (start end)
  "Returns a list of all sexps in region."
  (save-restriction
    (narrow-to-region start end)
    (check-parens)
    (let ((all-bounds (let ((out ()))
                        (while (let ((e (scan-sexps start 1)))
                                 (if e
                                     (let ((s (scan-sexps e -1)))
                                       (if s
                                           (progn (setq out (cons (cons s e) out))
                                                  (setq start e))
                                         nil))
                                   nil)))
                        (reverse out))))
      (mapcar (lambda (bounds)
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
              all-bounds))))

;;;; Configuration

(defgroup scrim nil
  "Scrim group"
  :prefix "scrim-"
  :group 'clojure
  :link '(url-link :tag "GitHub" "https://github.com/"))

(defcustom scrim-prompt-read-only t
  "If t, the prompt in the Scrim REPL buffer is read-only."
  :type 'boolean)

(defcustom scrim-prompt-regexp "^[^=> \n]+=> *"
  "Regexp for the Clojure prompt. Default should match the
default Clojure REPL prompt.
See https://clojure.github.io/clojure/clojure.main-api.html#clojure.main/repl
for customizing that prompt."
  :type 'regexp)

;;;; REPL buffer

(defvar scrim--buffer-name "*scrim*"
  "The name of the Scrim REPL buffer.")

(defun scrim-proc ()
  "Return the current Scrim REPL process, or nil if it doesn't
exist."
  (get-buffer-process scrim--buffer-name))

(defun scrim-clear-repl-buffer ()
  "Clear the Scrim REPL buffer."
  (interactive)
  (with-current-buffer scrim--buffer-name
    (comint-clear-buffer)
    (goto-char (point-max))))

(defun scrim-repl-buffer-end ()
  (interactive)
  (if (get-buffer scrim--buffer-name)
      (set-window-point (get-buffer-window scrim--buffer-name "visible")
                        (process-mark (scrim-proc)))
    (user-error "Not connected.")))

(defun scrim-show-or-hide-repl-buffer ()
  "Show the Scrim REPL buffer, if it exists and is not already
visible, or if it is visible, replace it with the previous
buffer."
  (interactive)
  (if (get-buffer scrim--buffer-name)
      (let ((window (get-buffer-window scrim--buffer-name "visible")))
        (if window
            (switch-to-prev-buffer window)
          (display-buffer scrim--buffer-name)))
    (user-error "Not connected.")))

(defun scrim-last-output ()
  "Returns the text between the last prompt and the current
prompt in the REPL."
  (with-current-buffer scrim--buffer-name
    (let* ((s (buffer-substring-no-properties comint-last-input-end (process-mark (scrim-proc))))
           ;; Remove any trailing prompt.
           (s (replace-regexp-in-string (concat scrim-prompt-regexp "\\'") "" s))
           ;; Remove any trailing newlines.
           (s (replace-regexp-in-string "\n+\\'" "" s)))
      s)))


;;;; Low-level, comint I/O

(defun scrim--send-indirectly (proc s)
  "Sends the string s to process proc by first writing s to the
process buffer and then sending it from there as if a human typed
it in."
  (with-current-buffer scrim--buffer-name
    ;; If point is at the end of the buffer, move it forward, otherwise leave it. This doesn't work
    ;; if point is within the previous output. I think comint adjusts point when the response is
    ;; received. This is supposed to be DWIM, but might be too magical.
    (let ((start (point))
          (end?  (= (point) (point-max))))
      (comint-goto-process-mark)
      (insert s)
      (comint-send-input)
      (unless end? (goto-char start)))))

(defun scrim--send-directly (proc s)
  "Sends the string s to process proc directly."
  (comint-simple-send proc s))

(defun scrim--send (proc s)
  (scrim--send-indirectly proc s))

(defun scrim-redirect-result-from-process (process command)
  "Send COMMAND to PROCESS. Return the output. Does not show input or output in Scrim REPL buffer.

Adapted from comint-redirect-results-list-from-process."
  (let ((output-buffer " *Scrim Redirect Work Buffer*")
	      results)
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process command output-buffer process nil t)
      ;; Wait for the process to complete
      (set-buffer (process-buffer process))
      (while (and (null comint-redirect-completed)
		              (accept-process-output process 60)))
      ;; Collect the output
      (set-buffer output-buffer)
      (let ((s (buffer-substring-no-properties (point-min) (point-max))))
        ;; Remove any trailing newlines
        (replace-regexp-in-string "\n+\\'" "" s)))))


;;;; High-level, Clojure I/O

(defun scrim-eval-region (start end)
  "Send each toplevel expression in the region bound by start and
end to the REPL process, one at a time. Note that toplevel here
is scoped to the region."
  (interactive "r")
  (mapc (lambda (sexp)
          ;; Give the process a chance to reply before the next input,
          ;; so that input and output are interleaved in the buffer.
          (sleep-for 0.05)
          (scrim--send (scrim-proc) sexp))
        (scrim-sexps-in-region start end)))

(defun scrim-eval-buffer ()
  "Send each expression in the accessible portion of current
buffer to the REPL process, one at a time. You can use C-x n n to
limit the part of buffer to be evaluated."
  (interactive)
  (scrim-eval-region (point-min) (point-max)))

(defun scrim-eval-around-or-previous-sexp ()
  (interactive)
  (let ((s (scrim-outer-around-or-previous-sexp)))
    (if s
        (scrim--send (scrim-proc) s)
      (user-error "No expression."))))

(defun scrim-eval-previous-sexp ()
  "Send the expression nearest to point to the REPL
process."
  (interactive)
  (let ((s (scrim-previous-sexp)))
    (if s
        (scrim--send (scrim-proc) s)
      (user-error "No expression."))))

(defun scrim-quit ()
  "Send EOF to the Scrim REPL process."
  (interactive)
  (if (get-buffer scrim--buffer-name)
      (with-current-buffer scrim--buffer-name
        (comint-send-eof))
    (user-error "Not connected.")))


;;;; Keymaps

(defvar scrim-repl-map
  (let ((map (define-prefix-command 'scrim-repl-map)))
    (define-key map (kbd "C-d")   #'scrim-send-doc)
    (define-key map (kbd "C-s")   #'scrim-send-source)
    (define-key map (kbd "C-S-a") #'scrim-send-apropos)
    (define-key map (kbd "C-S-d") #'scrim-send-dir)
    (define-key map (kbd "C-p")   #'scrim-send-pst)
    map)
  "Bindings for functions in clojure.repl.")

(defvar scrim-pprint-map
  (let ((map (define-prefix-command 'scrim-pprint-map)))
    (define-key map (kbd "C-p") #'scrim-send-pp)
    map)
  "Bindings for functions in clojure.pprint.")

(defvar scrim-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-q")   #'scrim-quit)
    (define-key map (kbd "C-c C-S-o") #'scrim-clear-repl-buffer)

    (define-key map (kbd "C-c C-e")   #'scrim-eval-previous-sexp)
    (define-key map (kbd "C-c C-c")   #'scrim-eval-around-or-previous-sexp)

    (define-key map (kbd "C-c C-r")   'scrim-repl-map)
    (define-key map (kbd "C-c C-p")   'scrim-pprint-map)
    map))

(defvar scrim-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-S-c") #'scrim-connect)
    (define-key map (kbd "C-c C-q")   #'scrim-quit)

    (define-key map (kbd "C-c C-z")   #'scrim-show-or-hide-repl-buffer)
    (define-key map (kbd "C-c C-S-o") #'scrim-clear-repl-buffer)
    (define-key map (kbd "C-c C-S-e") #'scrim-repl-buffer-end)

    (define-key map (kbd "C-c C-e")   #'scrim-eval-previous-sexp)
    (define-key map (kbd "C-c C-c")   #'scrim-eval-around-or-previous-sexp)
    (define-key map (kbd "C-c C-b")   #'scrim-eval-buffer)
    (define-key map (kbd "C-c C-S-r") #'scrim-eval-region)

    (define-key map (kbd "C-c C-l")   #'scrim-send-load-file)
    (define-key map (kbd "C-c r")     #'scrim-send-require)
    (define-key map (kbd "C-c C-n")   #'scrim-send-in-ns)
    (define-key map (kbd "C-c C-a")   #'scrim-send-arglists)
    (define-key map (kbd "C-c C-m")   #'scrim-send-macroexpand)

    (define-key map (kbd "C-c C-r")   'scrim-repl-map)
    (define-key map (kbd "C-c C-p")   'scrim-pprint-map)
    map))


;;;; Modes

;;;###autoload
(define-minor-mode scrim-minor-mode
  "Minor mode for interacting with the Scrim REPL buffer.

\\{scrim-minor-mode-map}"
  :lighter " Scrim"
  :keymap scrim-minor-mode-map
  (setq-local comint-input-sender 'scrim--send)
  (add-hook 'xref-backend-functions #'scrim--xref-backend nil t)
  (add-hook 'completion-at-point-functions #'scrim--tags-completion-at-point nil t)
  (setq-local eldoc-documentation-function 'scrim--eldoc-function))

(define-derived-mode scrim-mode comint-mode "scrim"
  "Major mode for a Clojure REPL.

\\{scrim-mode-map}"
  (setq comint-prompt-regexp scrim-prompt-regexp)
  (setq mode-line-process '(":%s"))
  (setq-local comint-prompt-read-only scrim-prompt-read-only)
  (ansi-color-for-comint-mode-on)
  (setq-local eldoc-documentation-function 'scrim--eldoc-function))


;;;; Starting

;;;###autoload
(defun scrim (program)
  "Launch a Scrim REPL buffer, running PROGRAM.

PROGRAM should be one of the following:
- a string, denoting an executable program to create via
  ‘start-file-process’
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via ‘open-network-stream’

Note that, if PROGRAM is a string, there is a bug where any
string longer than 4096 characters sent to the process will be
truncated. Until we can determine how to work around that, it is
better to launch the program externally and use the TCP
connection to connect to it.

Some ways of launching an external Clojure process with a socket
server:

java -Dclojure.server.repl='{:port 5555 :accept clojure.core.server/repl}' -jar path-to-clojure-jar

clj -J-Dclojure.server.myrepl='{:port 5555,:accept,clojure.core.server/repl}'

JVM_OPTS='-Dclojure.server.myrepl={:port,5555,:accept,clojure.core.server/repl}' lein repl

Also, if PROGRAM is a string, the program will be run from the
directory of the current buffer. We need to determine how to
select a directory as the project root."
  (interactive (list (scrim--prompt "program" "clojure")))
  (if (get-buffer-process scrim--buffer-name)
      (user-error "Already connected.")
    (message "Starting Clojure REPL.")
    (display-buffer (get-buffer-create scrim--buffer-name))
    (make-comint-in-buffer "scrim" scrim--buffer-name program)
    (save-excursion
      (set-buffer scrim--buffer-name)
      (scrim-mode))))

(defcustom scrim-default-host "localhost"
  "The default host to connect to a REPL socket server."
  :type 'string
  :safe 'stringp)

(defcustom scrim-default-port 5555
  "The default port to connect to a REPL socket server."
  :type 'integer
  :safe 'integerp)

;;;###autoload
(defun scrim-connect (host port)
  "Same as (scrim '(host . port))."
  (interactive (list (scrim--prompt "host" scrim-default-host)
                     (scrim--prompt "port" scrim-default-port)))
  (scrim (cons host port)))


;;;; Commands that build common Clojure expressions, usually based on symbols near point, and send
;;;; them to the REPL.

(defmacro scrim--cmd (name prompt value-fn format-string error-msg)
  "Macro for defining simple commands that compose a Clojure
expression, usually based on some symbol near point, and send it
to the REPL process. If the function receives an optional prefix
argument, it will prompt for input."
  `(defun ,name (prompt)
     (interactive "P")
     (let ((arg (if prompt
                    (scrim--prompt ,prompt (funcall ,value-fn))
                  (funcall ,value-fn))))
       (if arg
           (scrim--send (scrim-proc) (format ,format-string arg))
         (user-error ,error-msg)))))

;;; core

(scrim--cmd scrim-send-require
            "require ns"
            'clojure-find-ns
            "(require '%s)"
            "Namespace not found")

(scrim--cmd scrim-send-in-ns
            "Set namespace to symbol"
            'clojure-find-ns
            "(in-ns '%s)"
            "Namespace not found")

(scrim--cmd scrim-send-arglists
            "arglists for fn"
            'scrim-current-function-symbol
            "(:arglists (meta (resolve '%s)))"
            "No function near point")

(defun scrim-send-macroexpand (&optional macro-1)
  (interactive "P")
  (let ((expr (scrim-previous-sexp)))
    (if expr
        (scrim--send (scrim-proc) (format (if macro-1
                                              "(macroexpand-1 '%s)"
                                            "(macroexpand '%s)")
                                          expr))
      (user-error "No sexp near point"))))

(defun scrim-send-load-file (file)
  "Sends (clojure.core/load-file file) to the REPL."
  (interactive (list (expand-file-name (read-file-name "file: " nil buffer-file-name t (file-name-nondirectory buffer-file-name)))))
  (comint-check-source file)
  (if file
      (scrim--send (scrim-proc) (format "(load-file \"%s\")" file))
    (user-error "No file found")))

;;; repl

(scrim--cmd scrim-send-doc
            "doc for symbol"
            'scrim-symbol-at-point
            "(clojure.repl/doc %s)"
            "No symbol near point")

(defun scrim-send-source (n)
  "Sends (clojure.repl/source n) to the REPL.

When called interactively, if the point is at a symbol, then it
will use that symbol. If point is not at a symbol, or if a prefix
is used, then it will prompt for a namespace and a symbol.

Uses process redirection to silently query the REPL for
namespaces and public symbols, which are then used in the
prompt."
  (interactive (let ((sym (scrim-symbol-at-point)))
                 (list
                  (if (and (null current-prefix-arg)
                           sym)
                      sym
                      (let* ((ns (clojure-find-ns))
                             (nss (read (scrim-redirect-result-from-process (scrim-proc) "(->> (all-ns) (map ns-name) (map name))")))
                             (ns (completing-read (if ns
                                                      (format "ns (default %s): " ns)
                                                    "ns: ")
                                                  nss
                                                  nil
                                                  t
                                                  nil
                                                  nil
                                                  ns))
                             (syms (read (scrim-redirect-result-from-process (scrim-proc) (format "(map first (ns-publics '%s))" ns))))
                             (sym (completing-read "sym: "
                                                   syms
                                                   nil
                                                   t)))
                        (string-join (list ns sym) "/"))))))
  (if n
      (scrim--send (scrim-proc) (format "(clojure.repl/source %s)" n))
    (user-error "No name found")))

(defun scrim-send-dir (nsname)
  "Sends (clojure.repl/dir nsname) to the REPL.

Uses process redirection to silently query the REPL for
namespaces, which are then used in the prompt."
  (interactive (list
                (let ((ns (clojure-find-ns))
                      (nss (read (scrim-redirect-result-from-process (scrim-proc) "(->> (all-ns) (map ns-name) (map name))"))))
                  (completing-read (if ns
                                       (format "ns (default %s): " ns)
                                     "ns: ")
                                   nss
                                   nil
                                   t
                                   nil
                                   nil
                                   ns))))
  (if nsname
      (scrim--send (scrim-proc) (format "(clojure.repl/dir %s)" nsname))
    (user-error "No namespace found")))

(defun scrim-send-apropos (str-or-pattern)
  (interactive (list (read-string "apropos for str-or-pattern: ")))
  (if (equal "" str-or-pattern)
      (user-error "You didn't specify a string or pattern")
    (scrim--send (scrim-proc)
                 (format "(doseq [v (sort (clojure.repl/apropos %s))] (println v))"
                         str-or-pattern))))

(defun scrim-send-pst ()
  (interactive)
  (scrim--send (scrim-proc) "(clojure.repl/pst)"))

;;; pretty print

(defun scrim-send-pp ()
  (interactive)
  (scrim--send (scrim-proc) "#?(:clj (clojure.pprint/pp) :cljs (cljs.pprint/pp))"))


;;;; scrim-db

(defvar scrim--db-filename ".scrim-db")

(defvar scrim--db '())

(defun scrim--db-get (db key)
  (alist-get key db nil nil #'string-equal))

(defun scrim--db-get-in (db &rest keys)
  "Finds a value in a nested alist, where all keys are strings."
  (let ((n (length keys)))
   (cond
    ((eq n 0) db)
    ((eq n 1) (scrim--db-get db (car keys)))
    (t        (apply #'scrim--db-get-in
                     (scrim--db-get db (car keys))
                     (cdr keys))))))

(defvar scrim--build-db-clj
  "(letfn [(clj->elisp [x]
          (clojure.walk/postwalk
           (fn [x]
             (cond
               (instance? clojure.lang.Namespace x) (str (ns-name x))
               (keyword? x) (name x)
               (symbol? x) (str x)
               (var? x) (-> (select-keys (meta x) [:arglists :file :column :line :ns])
                            (update :arglists (fn [x] (if x (str x) nil)))
                            (update :ns (comp str ns-name))
                            (update :file
                                    #(cond
                                       (nil? %) %
                                       (= % \"NO_SOURCE_PATH\") %
                                       :else (str (.getResource
                                                   (clojure.lang.RT/baseLoader)
                                                   %))))
                            clj->elisp)
               (boolean? x) (if (true? x) 't nil)
               (fn? x) (str x)
               (map-entry? x) x
               (map? x) (for [[k v] x] (list k '. v))
               (coll? x) (seq x)
               :else x))
           x))]
  (pr-str
    (clj->elisp
     (into {} (for [ns (all-ns)]
                [ns {'aliases (ns-aliases ns)
                     'refers  (->> (ns-refers ns)
                                   ;; To save space, only record the ns the symbol
                                   ;; belongs to. We can look up the rest of
                                   ;; the info in that namespace's publics.
                                   (map (fn [[k v]] [k (:ns (meta v))]))
                                   ;; Filter out clojure.core, because every ns
                                   ;; implicitly refers every symbol in it,
                                   ;; and that would increase the size of the
                                   ;; db significantly.
                                   (remove (fn [[k v]] (= 'clojure.core (ns-name v))))
                                   (into {}))
                     'publics (ns-publics ns)}])))))")

(defun scrim-build-db ()
  (interactive)
  (if (get-buffer scrim--buffer-name)
      (let ((progress-reporter (make-progress-reporter "Building database"))
            (db (scrim-redirect-result-from-process
                 (scrim-proc)
                 scrim--build-db-clj)))
        (setq scrim--db (read (read db)))
        (progress-reporter-done progress-reporter))
    (user-error "Not connected.")))

(defun scrim-save-db ()
  (interactive)
  (let ((f (concat (project-root (project-current t))
                   scrim--db-filename)))
    (with-temp-buffer
      (print scrim--db (current-buffer))
      (write-file f)
      (message "scrim db saved."))))

(defun scrim-load-db ()
  (interactive)
  (let ((f (concat (project-root (project-current t))
                   scrim--db-filename)))
    (if (file-exists-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (setq scrim--db (read (current-buffer)))
          (message "scrim db loaded."))
      (setq scrim--db '())
      (message "Could not load scrim db."))))

(defvar scrim--symbol-regexp "^\\(.*\\)/\\(.*\\)")

(defun scrim--parse-symbol (symbol)
  "Returns (ns . symbol). ns may be nil or an alias."
  (if (string-match scrim--symbol-regexp symbol)
    (let ((ns2 (match-string 1 symbol))
          (symbol2 (match-string 2 symbol)))
      (cons ns2 symbol2))
    (cons nil symbol)))

(defun scrim--parse-symbol-found-in-ns (ns symbol)
  "Returns (ns . symbol). ns may be nil or an alias.

The supplied ns is the ns where symbol was found. The returned ns
is the ns where symbol is defined.

This function depends on scrim--db being initialized."
  (let* ((parsed-symbol (scrim--parse-symbol symbol))
         (ns2 (car parsed-symbol))
         (sym (cdr parsed-symbol))
         (ns3 (if (null ns2)
                  (or (scrim--db-get-in scrim--db ns "refers" sym)
                      (and (scrim--db-get-in scrim--db ns "publics" symbol)
                           ns)
                      ;; scrim--build-db-clj elides symbols in clojure.core to
                      ;; save space, so we need to check for them here.
                      (and (scrim--db-get-in scrim--db "clojure.core" "publics" symbol)
                           "clojure.core"))
                (or (scrim--db-get-in scrim--db ns "aliases" ns2)
                    ns2))))
    (cons ns3 sym)))

(defun scrim--lookup-db-xref (ns symbol)
  (let ((sym (scrim--parse-symbol-found-in-ns ns symbol)))
    (scrim--db-get-in scrim--db (car sym) "publics" (cdr sym))))


;;;; eldoc

(defvar-local scrim--eldoc-cache nil)

(defun scrim--eldoc-function ()
  (when (not (nth 4 (syntax-ppss))) ; inside a comment?
    (when-let ((sym (or (scrim-current-function-symbol)
                        (scrim-symbol-at-point))))
      (when (string-match "^[a-zA-Z]" sym)
        (cond
         ((string= sym (car scrim--eldoc-cache)) (cdr scrim--eldoc-cache))
         (t (let* ((ns (clojure-find-ns))
                   (sym-alist (scrim--lookup-db-xref ns sym))
                   (s (when sym-alist
                        (if-let ((arglist (scrim--db-get (scrim--lookup-db-xref ns sym)
                                                         "arglists")))
                            (format "%s: %s"
                                    (propertize sym 'face 'font-lock-function-name-face)
                                    arglist)
                          (format "%s"
                                  (propertize sym 'face 'font-lock-function-name-face))))))
              (setq scrim--eldoc-cache (cons sym s))
              s)))))))


;;;; xref

(defun scrim--archive-extract (archive file)
  ;; Implementation based on archive-extract fn in arc-mode.
  (let* (;; arc-mode says these next two are usually `eq', except when
         ;; iname is the downcased ename. I don't know if that is
         ;; relevant to this implementation.
         (ename file)
         (iname file)
         (arcdir (file-name-directory archive))
         (arcname (file-name-nondirectory archive))
         (bufname (concat (file-name-nondirectory file) " (" arcname ")"))
         (read-only-p t)
         (arcfilename (expand-file-name (concat arcname ":" iname)))
         (buffer (get-buffer bufname)))
    (if (and buffer
             (string= (buffer-file-name buffer) arcfilename))
        buffer
      (setq bufname (generate-new-buffer-name bufname))
      (setq buffer (get-buffer-create bufname))
      (with-current-buffer buffer
        (let ((coding-system-for-read 'prefer-utf-8))
          (archive-zip-extract archive file))
        (setq buffer-file-name arcfilename)
        (setq buffer-file-truename (abbreviate-file-name buffer-file-name))
        (setq default-directory arcdir)
        (add-hook 'write-file-functions #'archive-write-file-member nil t)
        (archive-set-buffer-as-visiting-file ename)
        (setq buffer-read-only t)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)
        (setq buffer-saved-size (buffer-size))
        (normal-mode)
        (run-hooks 'archive-extract-hook))
      (archive-maybe-update t)
      buffer)))

(defclass scrim--xref-archive-location (xref-location)
  ((archive :type string :initarg :archive)
   (file :type string :initarg :file)
   (line :type fixnum :initarg :line)
   (column :type fixnum :initarg :column))
  :documentation "An archive location is an archive/file/line/column quadruple.
Line numbers start from 1 and columns from 0.")

(defun scrim--xref-make-archive-location (archive file line column)
  "Create and return a new `scrim--xref-archive-location'."
  (make-instance 'scrim--xref-archive-location :archive archive :file file :line line :column column))

(cl-defmethod xref-location-marker ((l scrim--xref-archive-location))
  (with-slots (archive file line column) l
    (with-current-buffer
        (scrim--archive-extract archive file)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (ignore-errors
            ;; xref location may be out of date; it may be past the
            ;; end of the current file, or the file may have been
            ;; deleted. Return a reasonable location; the user will
            ;; figure it out.
            (beginning-of-line line)
            (forward-char column))
          (point-marker))))))

(cl-defmethod xref-location-group ((l scrim--xref-archive-location))
  (with-slots (archive file) l
    (concat (file-name-nondirectory file) " (" archive ")")))

(defun scrim--get-xref (symbol file line column)
  "Retuns an xref for the given arguments. If the parameters
specify a location in a jar or zip file, it will attempt to
extract the file from the archive and load it into a buffer
before returning an xref."
  (cond ((string-match "^file:\\(.*\\)" file)
         (let ((file (match-string 1 file)))
           (xref-make (prin1-to-string symbol)
                      (xref-make-file-location file line column))))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" file)
         (let* ((archive (match-string 2 file))
                (file (match-string 3 file)))
           (xref-make (prin1-to-string symbol)
                      (scrim--xref-make-archive-location archive file line column))))))

(defun scrim--find-definition (symbol)
  "This function depends on scrim--db."
  (let* ((ns (clojure-find-ns))
         (alist (scrim--lookup-db-xref ns symbol))
         (file (scrim--db-get alist "file"))
         (line (scrim--db-get alist "line"))
         (column (scrim--db-get alist "column")))
    (when (and file (not (string-equal file "NO_SOURCE_PATH")))
      (when-let ((xref (scrim--get-xref symbol file line column)))
        (list xref)))))

(defun scrim--xref-backend () 'scrim)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql scrim)))
  (scrim-symbol-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql scrim)))
  ;; This supports find-definition and find-references.

  ;; TODO: Cache this table.
  (mapcan (lambda (ns)
            (mapcar (lambda (sym) (concat (car ns) "/" (car sym)))
                    (scrim--db-get ns "publics")))
          scrim--db))

(cl-defmethod xref-backend-identifier-completion-ignore-case ((_backend (eql scrim)))
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql scrim)) identifier)
  "This depends on scrim--db."
  (scrim--find-definition identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql scrim)) pattern)
  (let ((regexp (xref-apropos-regexp pattern)))
    (seq-remove #'null
                (mapcar (lambda (x)
                          (let* ((alist (cdr x))
                                 (symbol (car x))
                                 (file (scrim--db-get alist "file"))
                                 (line (scrim--db-get alist "line"))
                                 (column (scrim--db-get alist "column")))
                            (if (and file (not (string-equal file "NO_SOURCE_PATH")))
                                (when-let ((xref (scrim--get-xref symbol file line column)))
                                  xref)
                              ;; TODO: Determine how to handle vars that were
                              ;; evaluated in the REPL. If we use this bogus
                              ;; location xref, an error will be thrown if any
                              ;; of these are returned. If we use nil, the var
                              ;; will be silently elided. Maybe we should return
                              ;; a buffer xref to the REPL?

                              ;;(xref-make-bogus-location "NO_SOURCE_PATH")
                              nil)))
                        (seq-filter (lambda (x) (string-match regexp (car x)))
                                    (seq-remove #'null
                                                (apply #'append
                                                       (mapcar (lambda (ns) (scrim--db-get ns "publics"))
                                                               scrim--db))))))))

(cl-defmethod xref-backend-references ((_backend (eql scrim)) identifier)
  ;; This has some limitations:

  ;;   `identifier` must be a fully qualified symbol.

  ;;   Doesn't find symbols in jars, even if they are already extracted into
  ;;   buffers, because the location type for xref-match-item is
  ;;   xref-file-location.

  ;;   Doesn't detect references via :use.

  ;;   Doesn't find fully qualified symbols, unless they are also mentioned in
  ;;   the namespace's refers or aliases.

  ;;   Doesn't find symbols evaluated in the REPL.

  (let* ((parsed-symbol (scrim--parse-symbol identifier))
         (symbol-ns (car parsed-symbol))
         (symbol-name (cdr parsed-symbol)))
    (mapcan (lambda (ns)
              ;; HACK! Getting the file from the ns based on the first public
              ;; symbol in that ns.
              (let* ((file (seq-some (lambda (file)
                                       (if (string-prefix-p "file:" file)
                                           (string-remove-prefix "file:" file)
                                         nil))
                                     (mapcar
                                      (lambda (x) (scrim--db-get x "file"))
                                      (scrim--db-get ns "publics"))))
                     (refers (or (string-equal (scrim--db-get-in ns "refers" symbol-name) symbol-ns)
                                 (and (string-equal "clojure.core" symbol-ns)
                                      (scrim--db-get-in scrim--db "clojure.core" "publics" symbol-name))))
                     (alias (seq-some (lambda (x)
                                        (and (string-equal (cdr x) symbol-ns)
                                             (car x)))
                                      (scrim--db-get-in ns "aliases")))
                     (strings (seq-remove #'null
                                          (list (and refers symbol-name)
                                                (and alias (concat alias "/" symbol-name)))))
                     (regexp (when strings
                               (regexp-opt strings 'words)))) ;; Why doesn't 'symbols work?
                (when (and file regexp)
                  (xref-matches-in-files regexp (list file)))))
            scrim--db)))


;;;; Completion

;; TODO: Cache this?
;; TODO: Check if there is a function to optimize completion tables.
(defun scrim--tags-completion-at-point ()
  (let* ((sym (scrim-symbol-at-point))
         (start (beginning-of-thing 'symbol))
         (end (end-of-thing 'symbol))
         (props nil)
         (ns (clojure-find-ns))
         (ns-alist (scrim--db-get-in scrim--db ns))
         (publics (scrim--db-get ns-alist "publics"))
         (refers (scrim--db-get ns-alist "refers"))
         (aliases (scrim--db-get ns-alist "aliases"))
         (collection (nconc
                      (mapcar #'car (scrim--db-get-in scrim--db "clojure.core" "publics"))
                      (mapcar #'car publics)
                      (mapcar #'car refers)
                      (mapcan (lambda (x)
                                (let* ((alias (car x))
                                       (alias-ns (cdr x))
                                       (alias-publics (scrim--db-get-in scrim--db alias-ns "publics")))
                                  (mapcar (lambda (x) (concat alias "/" (car x)))
                                          alias-publics)))
                              aliases))))
    (cons start (cons end (cons collection props)))))

(provide 'scrim)

;;; scrim.el ends here
