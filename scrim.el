;; -*- lexical-binding: t; -*-

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

(require 'arc-mode)
(require 'cl-lib)
(require 'clojure-mode)
(require 'comint)
(require 'project)
(require 'subr-x)
(require 'thingatpt)
(require 'xref)


(defconst scrim-version "0.0.6-SNAPSHOT"
  "The current version of `Scrim'.")

;;;; Project support

(defvar scrim--project-root-file-names
  '("deps.edn" "project.clj" "build.boot"))

(defun scrim--project-find (dir)
  (when-let ((project-root (seq-some (lambda (name)
                                       (locate-dominating-file dir name))
                                     scrim--project-root-file-names)))
    (cons 'scrim project-root)))

(cl-defmethod project-root ((project (head scrim)))
  (cdr project))

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
  :link '(url-link :tag "GitHub" "https://github.com/austinhaas/scrim"))

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
  (interactive nil scrim-mode scrim-minor-mode)
  (with-current-buffer scrim--buffer-name
    (comint-clear-buffer)
    (goto-char (point-max))))

(defun scrim-repl-buffer-end ()
  (interactive nil scrim-mode scrim-minor-mode)
  (if (get-buffer scrim--buffer-name)
      (set-window-point (get-buffer-window scrim--buffer-name "visible")
                        (process-mark (scrim-proc)))
    (user-error "Not connected.")))

(defun scrim-show-or-hide-repl-buffer ()
  "Show the Scrim REPL buffer, if it exists and is not already
visible, or if it is visible, replace it with the previous
buffer."
  (interactive nil scrim-mode scrim-minor-mode)
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

;; TODO: Wrap all of the user input in one overlay.

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
      (save-excursion
        (let ((e (point)))
          (goto-char (process-mark (scrim-proc)))
          (let ((b (line-end-position)))
            (let ((ov (make-overlay b e)))
              (overlay-put ov 'scrim t)
              (overlay-put ov 'invisible 'scrim)
              (overlay-put ov 'isearch-open-invisible 'scrim--isearch-show)
              (overlay-put ov 'isearch-open-invisible-temporary 'scrim--isearch-show-temporary)
              ;; TODO: Lookup keybinding dynamically. See `substitute-command-keys'.
              ;;(overlay-put ov 'help-echo "\\[scrim--indent-line] to expand input.")
              ))))
      (comint-send-input)
      (unless end? (goto-char start)))))

(defun scrim--indent-line ()
  "In the REPL buffer, this attempts to show a hidden input
expression, otherwise invokes `clojure-indent-line'."
  (or (scrim-show-repl-input-at-point)
      (clojure-indent-line)))

(defun scrim--isearch-show (ov)
  "Delete overlay OV.

This function is meant to be used as the `isearch-open-invisible'
property of an overlay."
  (delete-overlay ov))

(defun scrim--isearch-show-temporary (ov hide-p)
  "Hide or show overlay OV depending on HIDE-P.
If HIDE-P is non-nil, overlay OV is hidden. Otherwise, OV is
shown.

This function is meant to be used as the `isearch-open-invisible-temporary'
property of an overlay."
  (overlay-put ov 'invisible (and hide-p 'scrim)))

(defun scrim--overlay-at (position)
  "Return scrim overlay at POSITION, or nil if none to be found."
  ;; Implementation based on `hs-overlay-at'.
  (let ((overlays (overlays-at position))
        ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (and (overlay-get ov 'scrim) ov)
            overlays (cdr overlays)))
    found))

(defun scrim-show-repl-input-at-point ()
  (interactive)
  ;; Very simplified version of `hs-show-block'. See that implementation, if
  ;; more is needed.
  (when-let ((ov (scrim--overlay-at (line-end-position))))
    (delete-overlay ov)
    (message "Showing input... done")))

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
  (interactive "r" scrim-minor-mode)
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
  (interactive nil scrim-minor-mode)
  (scrim-eval-region (point-min) (point-max)))

(defun scrim-eval-around-or-previous-sexp ()
  (interactive nil scrim-minor-mode)
  (let ((s (scrim-outer-around-or-previous-sexp)))
    (if s
        (scrim--send (scrim-proc) s)
      (user-error "No expression."))))

(defun scrim-eval-previous-sexp ()
  "Send the expression nearest to point to the REPL
process."
  (interactive nil scrim-minor-mode)
  (let ((s (scrim-previous-sexp)))
    (if s
        (scrim--send (scrim-proc) s)
      (user-error "No expression."))))

(defun scrim-quit ()
  "Send EOF to the Scrim REPL process."
  (interactive nil scrim-minor-mode)
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
  (setq-local eldoc-documentation-function 'scrim--eldoc-function)
  (add-hook 'completion-at-point-functions #'scrim--tags-completion-at-point nil t)
  (add-hook 'project-find-functions #'scrim--project-find nil t)
  (add-hook 'xref-backend-functions #'scrim--xref-backend nil t))

(define-derived-mode scrim-mode comint-mode "scrim"
  "Major mode for a Clojure REPL.

\\{scrim-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (setq comint-prompt-regexp scrim-prompt-regexp)
  (setq mode-line-process '(":%s"))
  (setq-local comint-prompt-read-only scrim-prompt-read-only)
  ;; eldoc currently works for symbols in clojure.core and fully-qualified
  ;; symbols, but nothing else because it doesn't know which ns it is in.
  ;; Even if we knew the current ns, the repl buffer has input from many
  ;; namespaces that might not be current.
  (setq-local eldoc-documentation-function 'scrim--eldoc-function)
  (setq-local indent-line-function #'scrim--indent-line)
  (add-to-invisibility-spec '(scrim . t)))


;;;; Starting

;;;###autoload
(defun scrim (program)
  "Launch a Scrim REPL buffer, running PROGRAM.

PROGRAM should be one of the following:
- a string, denoting an executable program to create via
  ‘start-file-process’
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via ‘open-network-stream’"
  (interactive (list (read-string "program: " "clojure")))
  (if (get-buffer-process scrim--buffer-name)
      (user-error "Already connected.")
    (message "Starting a Clojure REPL.")
    (let ((default-directory (project-root (project-current t)))
          ;; Binding process-connection-type to nil causes the communication with
          ;; the subprocess to use a pipe rather than a pty. Without this,
          ;; expressions longer than 1024 bytes cannot be sent to the subprocess.
          (process-connection-type nil))
      (display-buffer (get-buffer-create scrim--buffer-name))
      (make-comint-in-buffer "scrim" scrim--buffer-name program)
      (save-excursion
        (set-buffer scrim--buffer-name)
        (scrim-mode)))))

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
  (interactive (list (read-string "host: " scrim-default-host)
                     (read-number "port: " scrim-default-port)))
  (scrim (cons host port)))


;;;; Commands that build common Clojure expressions, usually based on symbols near point, and send
;;;; them to the REPL.

;;; core

(defun scrim-send-require (ns)
  (interactive (list (read-string "require ns: " (clojure-find-ns)))
               scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) (format "(require '%s)" ns)))

(defun scrim-send-in-ns (ns)
  (interactive (list (read-string "in ns: " (clojure-find-ns)))
               scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) (format "(in-ns '%s)" ns)))

(defun scrim-send-arglists (fn)
  (interactive (list (read-string "arglists for fn: " (scrim-current-function-symbol)))
               scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) (format "(:arglists (meta (resolve '%s)))" fn)))

(defun scrim-send-macroexpand (&optional macro-1)
  (interactive "P" scrim-mode scrim-minor-mode)
  (let ((expr (scrim-previous-sexp)))
    (if expr
        (scrim--send (scrim-proc) (format (if macro-1
                                              "(macroexpand-1 '%s)"
                                            "(macroexpand '%s)")
                                          expr))
      (user-error "No sexp near point"))))

(defun scrim-send-load-file (file)
  "Sends (clojure.core/load-file file) to the REPL."
  (interactive (list (expand-file-name
                      (read-file-name "file: " nil buffer-file-name t (file-name-nondirectory buffer-file-name)))
                     scrim-mode scrim-minor-mode))
  (comint-check-source file)
  (if file
      (scrim--send (scrim-proc) (format "(load-file \"%s\")" file))
    (user-error "No file found")))

;;; repl

(defun scrim-send-doc (symbol)
  (interactive (list (read-string "doc for symbol: " (scrim-symbol-at-point)))
               scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) (format "(clojure.repl/doc %s)" symbol)))

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
                        (string-join (list ns sym) "/")))))
               scrim-mode scrim-minor-mode)
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
                                   ns)))
               scrim-mode scrim-minor-mode)
  (if nsname
      (scrim--send (scrim-proc) (format "(clojure.repl/dir %s)" nsname))
    (user-error "No namespace found")))

(defun scrim-send-apropos (str-or-pattern)
  (interactive (list (read-string "apropos for str-or-pattern: "))
               scrim-mode scrim-minor-mode)
  (if (equal "" str-or-pattern)
      (user-error "You didn't specify a string or pattern")
    (scrim--send (scrim-proc)
                 (format "(doseq [v (sort (clojure.repl/apropos %s))] (println v))"
                         str-or-pattern))))

(defun scrim-send-pst ()
  (interactive nil scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) "(clojure.repl/pst)"))

;;; pretty print

(defun scrim-send-pp ()
  (interactive nil scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) "#?(:clj (clojure.pprint/pp) :cljs (cljs.pprint/pp))"))


;;;; scrim-db

;; The idea is to create an offline database of Clojure symbol metadata that can
;; be used to support features like find-definition and eldoc.

(defvar scrim--db nil)

(defvar scrim--db-filename ".scrim-db")

(defun scrim--db-file-path ()
  (concat (project-root (project-current t))
          scrim--db-filename))

(defvar scrim--build-db-clj
  ;; This produces a clojure value that can be read by emacs lisp.
  "(letfn [(alist [xs] (for [[k v] xs] (list k '. v)))]
  (alist (for [ns (all-ns)]
           [(str (ns-name ns))
            (alist
             {\"aliases\"
              (alist
               (for [[k v] (ns-aliases ns)]
                 [(name k) (str (ns-name v))]))

              \"refers\"
              (alist
               (for [[k v] (ns-refers ns)
                     :let  [ns-name (ns-name (:ns (meta v)))]
                     ;; To save space.
                     :when (not= 'clojure.core ns-name)]
                 [(name k) (str ns-name)]))

              \"interns\"
              (alist
               (for [[k v] (ns-interns ns)]
                 [(name k) (alist
                            (let [m (meta v)]
                              {\"arglists\" (when-let [x (get m :arglists)] (str x))
                               \"file\"     (when-let [x (get m :file)]
                                            (case x
                                              \"NO_SOURCE_PATH\" x
                                              (str (.getResource
                                                    (clojure.lang.RT/baseLoader)
                                                    x))))
                               \"column\"   (get m :column)
                               \"line\"     (get m :line)
                               \"name\"     (str (get m :name))
                               \"ns\"       (str (ns-name (get m :ns)))}))]))})])))")

(defun scrim-build-db ()
  ""
  (interactive nil scrim-mode scrim-minor-mode)
  (if (get-buffer scrim--buffer-name)
      (let ((progress-reporter (make-progress-reporter "Building database"))
            (db (scrim-redirect-result-from-process (scrim-proc)
                                                    scrim--build-db-clj)))
        (setq scrim--db (read db))
        (progress-reporter-done progress-reporter))
    (user-error "Not connected.")))

(defun scrim-save-db ()
  (interactive nil scrim-mode scrim-minor-mode)
  (message "Saving scrim db...")
  (with-temp-buffer
    (print scrim--db (current-buffer))
    (write-file (scrim--db-file-path)))
  (message "Saving scrim db...done"))

(defun scrim-load-db ()
  (interactive nil scrim-mode scrim-minor-mode)
  (message "Loading scrim db...")
  (let ((f (scrim--db-file-path)))
    (if (file-exists-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (setq scrim--db (read (current-buffer)))
          (message "Loading scrim db...done"))
      (message "Could not load scrim db."))))

(defun scrim--get (alist key &optional default)
  (if (listp alist)
      (alist-get key alist default nil #'string-equal)
    default))

(defun scrim--get-in (alist keys &optional default)
  "Finds a value in a nested alist, where all keys are strings."
  (if keys
      (let* ((sentinel (gensym))
             (result (scrim--get alist (car keys) sentinel)))
        (if (eq result sentinel)
            default
          (scrim--get-in result (cdr keys) default)))
    alist))

(defvar scrim--symbol-regexp "^\\(.*\\)/\\(.*\\)")

(defun scrim--parse-symbol (symbol)
  "Parse symbol. Returns (symbol-ns . symbol-name). symbol-ns may
be a fully-qualified namespace, nil, or an alias."
  (if (string-match scrim--symbol-regexp symbol)
      (cons (match-string 1 symbol)
            (match-string 2 symbol))
    (cons nil symbol)))

(defun scrim--lookup-symbol (current-ns symbol)
  "Returns the metadata alist associated with symbol in
scrim--db. The symbol is looked up in the context of
current-ns. For example, if symbol is str/trim, where str is an
alias for clojure.string in current-ns, then the metadata
associated with clojure.string/trim will be returned.

Both args are strings."
  (let* ((parsed-symbol (scrim--parse-symbol symbol))
         (symbol-ns-local (car parsed-symbol))
         (symbol-name (cdr parsed-symbol)))
    (if (null symbol-ns-local)
        ;; Symbol does not have a namespace component.
        (or
         ;; Symbol was refer'd.
         (when-let ((symbol-ns (scrim--get-in scrim--db (list current-ns "refers" symbol-name))))
           (scrim--get-in scrim--db (list symbol-ns "interns" symbol-name)))
         ;; Symbol is in the current ns.
         (scrim--get-in scrim--db (list current-ns "interns" symbol-name))
         ;; Symbol is in clojure.core. scrim--build-db-clj elides symbols in
         ;; clojure.core to save space, so we need to check for it explicitly.
         (scrim--get-in scrim--db (list "clojure.core" "interns" symbol-name)))
      ;; Symbol has a namespace component.
      (or
       ;; Alias
       (when-let ((symbol-ns (scrim--get-in scrim--db (list current-ns "aliases" symbol-ns-local))))
         (scrim--get-in scrim--db (list symbol-ns "interns" symbol-name)))
       ;; Fully-qualified
       (scrim--get-in scrim--db (list symbol-ns-local "interns" symbol-name))))))


;;;; eldoc

(defvar-local scrim--eldoc-cache nil)

(defun scrim--eldoc-function ()
  "This function depends on scrim--db."
  (when (not (nth 4 (syntax-ppss)))    ; inside a comment?
    (when-let ((sym (or (scrim-current-function-symbol)
                        (scrim-symbol-at-point))))
      (when (string-match "^[a-zA-Z]" sym)
        (cond
         ((string= sym (car scrim--eldoc-cache)) (cdr scrim--eldoc-cache))
         (t (let* ((ns (clojure-find-ns))
                   (alist (scrim--lookup-symbol ns sym))
                   (s (when alist
                        (if-let ((arglist (scrim--get alist "arglists")))
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
         (alist (scrim--lookup-symbol ns symbol))
         (file (scrim--get alist "file"))
         (line (scrim--get alist "line"))
         (column (scrim--get alist "column")))
    (when (and file (not (string-equal file "NO_SOURCE_PATH")))
      (when-let ((xref (scrim--get-xref symbol file line column)))
        (list xref)))))

(defun scrim--xref-backend () 'scrim)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql scrim)))
  (scrim-symbol-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql scrim)))
  "This implementation depends on scrim--db."
  ;; This supports find-definition and find-references.

  ;; TODO: Cache this table.
  (mapcan (lambda (ns)
            (mapcar (lambda (sym) (concat (car ns) "/" (car sym)))
                    (scrim--get ns "interns")))
          scrim--db))

(cl-defmethod xref-backend-identifier-completion-ignore-case ((_backend (eql scrim)))
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql scrim)) identifier)
  "This implementation depends on scrim--db."
  (scrim--find-definition identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql scrim)) pattern)
  "This implementation depends on scrim--db."
  (let ((regexp (xref-apropos-regexp pattern)))
    (seq-remove #'null
                (mapcar (lambda (x)
                          (let* ((alist (cdr x))
                                 (symbol (car x))
                                 (file (scrim--get alist "file"))
                                 (line (scrim--get alist "line"))
                                 (column (scrim--get alist "column")))
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
                                                       (mapcar (lambda (ns) (scrim--get ns "interns"))
                                                               scrim--db))))))))

(cl-defmethod xref-backend-references ((_backend (eql scrim)) identifier)
  "This implementation depends on scrim--db."
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
                                      (lambda (x) (scrim--get x "file"))
                                      (scrim--get ns "interns"))))
                     (refers (or (string-equal (scrim--get-in ns (list "refers" symbol-name)) symbol-ns)
                                 (and (string-equal "clojure.core" symbol-ns)
                                      (scrim--get-in scrim--db (list "clojure.core" "interns" symbol-name)))))
                     (alias (seq-some (lambda (x)
                                        (and (string-equal (cdr x) symbol-ns)
                                             (car x)))
                                      (scrim--get ns "aliases")))
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
  "This function depends on scrim--db."
  (let* ((sym (scrim-symbol-at-point))
         (start (beginning-of-thing 'symbol))
         (end (end-of-thing 'symbol))
         (props nil)
         (ns (clojure-find-ns))
         (ns-alist (scrim--get scrim--db ns))
         (interns (scrim--get ns-alist "interns"))
         (refers (scrim--get ns-alist "refers"))
         (aliases (scrim--get ns-alist "aliases"))
         (collection (nconc
                      (mapcar #'car (scrim--get-in scrim--db (list "clojure.core" "interns")))
                      (mapcar #'car interns)
                      (mapcar #'car refers)
                      (mapcan (lambda (x)
                                (let* ((alias (car x))
                                       (alias-ns (cdr x))
                                       (alias-interns (scrim--get-in scrim--db (list alias-ns "interns"))))
                                  (mapcar (lambda (x) (concat alias "/" (car x)))
                                          alias-interns)))
                              aliases))))
    (cons start (cons end (cons collection props)))))

(provide 'scrim)

;;; scrim.el ends here
