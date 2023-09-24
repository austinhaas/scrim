;;; scrim.el --- Simple Clojure REPL Interaction Mode       -*- lexical-binding: t; -*-

;; Copyright © 2023 Austin Haas
;;
;; Author: Austin Haas <austin@pettomato.com>
;; URL: http://github.com/austinhaas/scrim
;; Version: 0.0.10-SNAPSHOT

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

;; Dependencies that are included with Emacs

(require 'cl-lib)
(require 'comint)
(require 'lisp-mnt)
(require 'project)
(require 'subr-x)
(require 'thingatpt)

;; Additional dependencies

(require 'clojure-mode) ; https://github.com/clojure-emacs/clojure-mode/


(defconst scrim-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `scrim'.")


;;;; Project support

(defun scrim-project-root ()
  "Return root directory of the current project."
  (or (clojure-project-dir default-directory)
      (project-root (project-current t))))


;;;; Functions that extract expressions from Clojure buffers

(defun scrim-symbol-at-point ()
  "Return the symbol at point."
  (when (and (not (nth 3 (syntax-ppss)))  ; Not inside a string.
             (not (nth 4 (syntax-ppss)))) ; Not inside a comment.
    (thing-at-point 'symbol t)))

(defun scrim-last-sexp ()
  "Return the sexp before point."
  (save-excursion
    (let ((start (point)))
      (backward-sexp)
      (let ((beginning (point)))
        (forward-sexp)
        (let ((end (point)))
          (when (<= end start)
            (buffer-substring-no-properties beginning end)))))))

(defun scrim--beginning-of-sexp ()
  "Move to the beginning of current sexp. Return the number of
nested sexp the point was over or after."
  ;; Based on `elisp--beginning-of-sexp`
  (let ((ppss (syntax-ppss))
        (parse-sexp-ignore-comments t)
	      (num-skipped-sexps 0))
    (when (< 0 (nth 0 ppss)) ;; Must be inside at least one pair of parens.
      (condition-case _
	        (progn
	          ;; First account for the case the point is directly over a
	          ;; beginning of a nested sexp.
	          (condition-case _
	              (let ((p (point)))
		              (forward-sexp -1)
		              (forward-sexp 1)
		              (when (< (point) p)
		                (setq num-skipped-sexps 1)))
	            (error))
            ;; Move out of any strings.
            (when-let ((pos (nth 8 ppss)))
              (goto-char pos)
              (forward-sexp 1))
	          (while
	              (let ((p (point)))
		              (forward-sexp -1)
		              (when (< (point) p)
		                (setq num-skipped-sexps (1+ num-skipped-sexps))))))
        (error))
      ;; `elisp--beginning-of-sexp` stops at the pos just inside the paren, but I
      ;; want this to be consistent with `beginning-of-defun` and move point to
      ;; the opening paren.
      (backward-char)
      num-skipped-sexps)))

(defun scrim--current-function-info ()
  "Return a list of current function name and argument index."
  ;; Based on `elisp--fnsym-in-current-sexp`
  (save-excursion
    (when-let ((n (scrim--beginning-of-sexp)))
      (let ((argument-index (1- n)))
        ;; If we are at the beginning of function name, this will be -1.
        (when (< argument-index 0)
          (setq argument-index 0))
        (forward-char)
        (list (scrim-symbol-at-point) argument-index)))))

(defun scrim-current-function-symbol ()
  "Return the symbol in function position in the sexp around
point."
  (car (scrim--current-function-info)))

(defun scrim-current-sexp ()
  "Return sexp around point."
  (save-excursion
    (let ((start (point)))
      (scrim--beginning-of-sexp)
      (forward-sexp)
      (let ((end (point)))
        (backward-sexp)
        (let ((beginning (point)))
          (when (< beginning start end)
            (buffer-substring-no-properties beginning end)))))))

(defun scrim-top-level-sexp ()
  "Return the top-level sexp around point."
  (save-excursion
    (let ((start (point)))
      (beginning-of-defun)
      (forward-sexp)
      (let ((end (point)))
        (backward-sexp)
        (let ((beginning (point)))
          (when (< beginning start end)
            (buffer-substring-no-properties beginning end)))))))

(defun scrim-top-level-or-last-sexp ()
  "Return the outer sexp around point, if point is inside a sexp,
  otherwise return the sexp before point."
  (or (scrim-top-level-sexp)
      (scrim-last-sexp)))

(defun scrim-sexps-in-region (start end)
  "Return a list of all sexps in region."
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
  :link '(url-link :tag "GitHub" "https://github.com/austinhaas/scrim")
  :link '(emacs-commentary-link :tag "Commentary" "scrim"))

(defcustom scrim-prompt-read-only t
  "If t, the prompt in the Scrim REPL buffer is read-only."
  :type 'boolean
  :safe 'booleanp)

;; (defcustom scrim-prompt-regexp "^[^=> \n]+=> *"
;;   "Regexp for the Clojure prompt. Default should match the
;; default Clojure REPL prompt.
;; See https://clojure.github.io/clojure/clojure.main-api.html#clojure.main/repl
;; for customizing that prompt."
;;   :type 'regexp)

(defcustom scrim-prompt-regexp "^"
  "Regexp for the Clojure prompt. Default should match the
default Clojure REPL prompt.
See https://clojure.github.io/clojure/clojure.main-api.html#clojure.main/repl
for customizing the Clojure REPL prompt."
  :type 'regexp
  :safe 'stringp)


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
  "Move point to the end of the Scrim REPL buffer."
  (interactive nil scrim-mode scrim-minor-mode)
  (if (scrim-proc)
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
  "Return the text between the last prompt and the current prompt
in the REPL."
  (when (scrim-proc)
    (with-current-buffer scrim--buffer-name
      (let* ((s (buffer-substring-no-properties comint-last-input-end (process-mark (scrim-proc))))
             ;; Remove any trailing prompt.
             (s (replace-regexp-in-string (concat scrim-prompt-regexp "\\'") "" s))
             ;; Remove any trailing newlines.
             (s (replace-regexp-in-string "\n+\\'" "" s)))
        s))))


;;;; Low-level, comint I/O

(defun scrim--indent-line ()
  "If point is near a hidden input expression in the REPL show the expression,
otherwise indent the line via `clojure-indent-line'."
  (or (scrim-show-repl-input-at-point)
      (clojure-indent-line)))

(defun scrim--isearch-show (ov)
  "Delete overlay OV.

This function is meant to be used as the `isearch-open-invisible'
property of an overlay."
  (delete-overlay ov))

(defun scrim--isearch-show-temporary (ov hide-p)
  "Hide or show overlay OV depending on HIDE-P. If HIDE-P is
non-nil, overlay OV is hidden. Otherwise, OV is shown.

This function is meant to be used as the `isearch-open-invisible-temporary'
property of an overlay."
  (overlay-put ov 'invisible (and hide-p 'scrim)))

(defun scrim--invisible-overlay-at (position)
  "Return invisible scrim overlay at POSITION, or nil if none to be found."
  ;; Implementation based on `hs-overlay-at'.
  (seq-find (lambda (ov) (and (overlay-get ov 'scrim)
                              (overlay-get ov 'invisible)))
            (overlays-at position)))

(defun scrim-show-repl-input-at-point ()
  (interactive)
  ;; Very simplified version of `hs-show-block'. See that implementation, if
  ;; more is needed.
  (when-let ((ov (or (scrim--invisible-overlay-at (line-end-position))
                     ;; Based on `help-at-pt-string'.
                     (save-excursion
                       (goto-char (line-end-position))
                       (backward-char)
                       (scrim--invisible-overlay-at (point))))))
    (delete-overlay ov)
    (message "Showing input... done")))

(defun scrim--add-repl-input-overlay (start end)
  (save-excursion
    (goto-char start)
    ;; One overlay for all of the input.
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'scrim t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'kbd-help "<tab> to expand input"))
    ;; One overlay to hide everything after the first line.
    (let ((ov (make-overlay (line-end-position) end)))
      (overlay-put ov 'scrim t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'invisible 'scrim)
      (overlay-put ov 'isearch-open-invisible 'scrim--isearch-show)
      (overlay-put ov 'isearch-open-invisible-temporary 'scrim--isearch-show-temporary))))

(defun scrim--send-indirectly (process string)
  "Send STRING to PROCESS by first writing STRING to the process
buffer and then sending it from there as if a user typed it in."
  ;; The management of point is derived from
  ;; https://emacs.stackexchange.com/a/12346 and `append-to-buffer'.
  (if process
      (let* ((buffer (process-buffer process))
             (windows (get-buffer-window-list buffer t t)))
        (save-excursion
          (with-current-buffer buffer
            ;; If point is at the end of the buffer, move it forward, otherwise leave it. This doesn't work
            ;; if point is within the previous output. I think comint adjusts point when the response is
            ;; received. This is supposed to be DWIM, but might be too magical.
            (let ((point (point))
                  (point-at-max-p (= (point) (point-max))))
              (comint-goto-process-mark)
              (let ((input-start (point)))
                (insert string)
                (let ((input-end (point)))
                  ;; Need to send the input before adding the overlay,
                  ;; because `comint-send-input' removes overlays (and
                  ;; text properties) since Emacs commit 4268d9a2b6b.
                  (comint-send-input)
                  (put-text-property input-start input-end 'read-only t)
                  (scrim--add-repl-input-overlay input-start input-end)))
              (unless point-at-max-p
                (goto-char point))
              (dolist (window windows)
                (when (= (window-point window) point)
                  (set-window-point window (point))))))))
    (user-error "Not connected.")))

(defun scrim--send-indirectly-prepl (process string)
  (with-current-buffer scrim--buffer-name
    (let ((start (point))
          (end?  (= (point) (point-max))))
      (comint-goto-process-mark)
      (insert string)
      (comint-send-input)
      (unless end? (goto-char start)))))

(defun scrim--send-directly (process string)
  "Send STRING to PROCESS directly."
  (if process
      (comint-simple-send process string)
    (user-error "Not connected.")))

(defun scrim--send (process string)
  (scrim--send-indirectly-prepl process string))

(defun scrim--parse-prepl-result (s)
  (message "scrim--parse-prepl-result: %s" s)
  (with-temp-buffer
    (clojure-mode)
    (insert s)
    (goto-char (point-min))
    (forward-sexp) ;; Necessary to avoid including trailing whitespace.
    (let* ((s (+ (point-min) 1))
           (e (- (point) 1))
           (xs (scrim-sexps-in-region s e)))
      (read (concat "(" (string-join xs " ") ")")))))

(defun scrim--parse-prepl-results (s)
  "Converts a string as returned from prepl to a list of plists."
  (with-temp-buffer
    (clojure-mode)
    (insert s)
    (let ((xs (split-string s "\n" t)))
      (mapcar (lambda (x)
                (scrim--parse-prepl-result x))
              xs))))

(defun scrim--redirect-result-from-process (process command)
  "Send COMMAND to PROCESS. Return the output. Does not show input
or output in Scrim REPL buffer.

Adapted from comint-redirect-results-list-from-process."

  (if process
      (let ((output-buffer " *Scrim Redirect Work Buffer*"))
        (with-current-buffer (get-buffer-create output-buffer)
          (erase-buffer)
          (comint-redirect-send-command-to-process command output-buffer process nil t)
          ;; Wait for the process to complete
          (set-buffer (process-buffer process))
          (while (and (null comint-redirect-completed)
		                  (accept-process-output process 60)))
          ;; Collect the output
          (set-buffer output-buffer)
          (let* ((s (buffer-substring-no-properties (point-min) (point-max)))
                 ;; Assuming one result!
                 (plist (car (scrim--parse-prepl-results s)))
                 (val (plist-get plist :val)))
            val)))
    (user-error "Not connected.")))

(defface scrim-prepl-raw-face
  '((t :background "#222222"
       :foreground "#444444"))
  "Face for raw."
  :group 'scrim-mode)

(defface scrim-prepl-ret-face
  '((t :background "#2e8b57"
       :foreground "#ffffc0"))
  "Face for ret."
  :group 'scrim-mode)

(defface scrim-prepl-exception-face
  '((t :background "#aa3333"
       :foreground "#ffffff"))
  "Face for exceptions."
  :group 'scrim-mode)

(defface scrim-prepl-out-face
  '((t :background "#ffa500"
       :foreground "#333333"))
  "Face for out."
  :group 'scrim-mode)

(defface scrim-prepl-err-face
  '((t :background "#332222"
       :foreground "#ffffff"))
  "Face for err."
  :group 'scrim-mode)

(defface scrim-prepl-tap-face
  '((t :background "yellow"
       :foreground "black"))
  "Face for tap."
  :group 'scrim-mode)

(defface scrim-prepl-prompt-face
  '((t :background "#4682b4"
       :foreground "#ffffc0"))
  "Face for prompt."
  :group 'scrim-mode)

;; (face-spec-set 'scrim-prepl-out-face
;;                '((t :background "#ffe4c4"
;;                     :foreground "#333333")))

(defun scrim-preoutput-filter (s)
  (message "scrim-preoutput-filter: <start>%s<end>" s)
  (let ((xs (scrim--parse-prepl-results s))
        ns)
    (concat
     (propertize s 'face 'scrim-prepl-raw-face) ;; For debugging.
     (string-join
      (mapcar (lambda (plist)
                (let* ((tag (plist-get plist :tag))
                       (val (plist-get plist :val)))
                  (setq ns (plist-get plist :ns))
                  (let ((s (cond
                            ((equal tag :ret) (if (equal 'true (plist-get plist :exception))
                                                  (propertize val 'face 'scrim-prepl-exception-face)
                                                (propertize val 'face 'scrim-prepl-ret-face)))
                            ((equal tag :out) (propertize val 'face 'scrim-prepl-out-face))
                            ((equal tag :err) (propertize val 'face 'scrim-prepl-err-face))
                            ((equal tag :tap) (propertize val 'face 'scrim-prepl-tap-face)))))
                    ;; Removing trailing newlines, but I don't like that we're altering the
                    ;; output. Maybe we can be smarter about grouping these values and not adding
                    ;; the newline below.
                    (replace-regexp-in-string "\n+\\'" "" s))))
              xs)
      "\n")
     "\n"
     (concat (propertize (format "[%s]" ns) 'face 'scrim-prepl-prompt-face)
             "\n"))))


;;;; High-level, Clojure I/O

(defun scrim-eval-region (start end)
  "Send each top-level expression in the region bound by start and
end to the REPL process, one at a time. Note that top-level is
constrained to the region."
  (interactive "r" scrim-minor-mode)
  (mapc (lambda (sexp)
          ;; Give the process a chance to reply before the next input, so that
          ;; input and output are interleaved in the buffer.

          ;; TODO: Consider creating a blocking send or some other way of
          ;; confirming a response before continuing. Maybe scrim-last-output
          ;; could be used.
          (sleep-for 0.05)
          (scrim--send (scrim-proc) sexp))
        (scrim-sexps-in-region start end)))

(defun scrim-eval-buffer ()
  "Send each expression in the accessible portion of current buffer
to the REPL process, one at a time. `narrow-to-region` can be
used to limit the part of buffer to be evaluated."
  (interactive nil scrim-minor-mode)
  (scrim-eval-region (point-min) (point-max)))

(defun scrim-eval-defn ()
  "Send the top-level expression containing point, or before point
if point is not inside a top-level expression, to the REPL
process."
  (interactive nil scrim-minor-mode)
  (if-let ((s (scrim-top-level-or-last-sexp)))
      (scrim--send (scrim-proc) s)
    (user-error "No expression.")))

(defun scrim-eval-last-sexp ()
  "Send the expression before point to the REPL process."
  (interactive nil scrim-minor-mode)
  (if-let ((s (scrim-last-sexp)))
      (scrim--send (scrim-proc) s)
    (user-error "No expression.")))

(defun scrim-eval-current-sexp ()
  "Send the expression around point to the REPL process."
  (interactive nil scrim-minor-mode)
  (if-let ((s (scrim-current-sexp)))
      (scrim--send (scrim-proc) s)
    (user-error "No expression.")))

(defun scrim-quit ()
  "Send EOF to the Scrim REPL process."
  (interactive nil scrim-minor-mode)
  (if (get-buffer scrim--buffer-name)
      (with-current-buffer scrim--buffer-name
        (comint-send-eof))
    (user-error "Not connected.")))


;;;; Helper functions that interact with the REPL.

;; These functions send Clojure expressions to the REPL for evaluation. The
;; expressions evaluate to Clojure data structures, which are then returned to
;; Emacs as a string. The data structures are compatible with Elisp, so they can
;; be read into the Elisp process.

;;; REPL-based completion

(defvar cljs-default-namespaces (list "cljs.core"
                                      "cljs.pprint"
                                      "cljs.repl"
                                      "cljs.spec.alpha"
                                      "cljs.spec.gen.alpha"
                                      "clojure.string"
                                      "cljs.test"
                                      "cljs.user"
                                      "clojure.walk")
  "A list of namespaces that ClojureScript will always have
available. This is intended to be used as a workaround to support
completion tables in ClojureScript, which doesn't have `all-ns'.")

(defun scrim--repl-get-all-namespaces ()
  "Query the REPL for a list of all namespace names in the current environment.

In cljs, this returns `cljs-default-namespaces', because cljs
doesn't have `all-ns'.

This is intended to be used to support completion, and shouldn't
be considered an exhaustive list."
  (read (scrim--redirect-result-from-process
         (scrim-proc)
         (format "#?(:clj (->> (all-ns) (map ns-name) (map name)) :cljs '%s)"
                 cljs-default-namespaces))))

(defun scrim--repl-get-all-namespaced-symbols ()
  "Query the REPL for a list of all namespaced symbols.

In cljs, this only returns results for namespaces in
`cljs-default-namespaces', because cljs doesn't have `all-ns'.

This is intended to be used to support completion, and shouldn't
be considered an exhaustive list."
  (read (scrim--redirect-result-from-process
         (scrim-proc)
         (format "#?(:clj
 (->> (all-ns)
     (mapcat (comp vals ns-interns))
     (map meta)
     (map #(str (:ns %%) \"/\" (:name %%))))
 :cljs (->> (list %s)
     (mapcat vals)
     (map meta)
     (map #(str (:ns %%) \"/\" (:name %%)))))"
            ;; In cljs, the argument to `ns-interns' must be quoted, so
            ;; we have to wrap each ns individually here.
            (string-join
             (mapcar (lambda (ns) (format "(ns-interns '%s)" ns))
                     cljs-default-namespaces)
             " ")))))

(defun scrim--repl-get-all-symbols-in-current-ns ()
  "Query the REPL for a list of all symbols that MAY BE in the ns
that corresponds to the current buffer. The list will include
simple symbols that are interned or refered, all aliased symbols
that could be in the current ns, and imports.

Note that the symbols may or may not actually appear in the
buffer/namespace. We're just looking at what is possible given
the currently loaded state of the namespace. For example, if
`clojure.string' is aliased as `str', then `str/split' will be
included.

This assumes that the ns in the current buffer is loaded. If the
buffer ns hasn't been loaded, then this will return nil.

In cljs, refered and aliased symbols are not included, because
cljs doesn't have `ns-refers' and `ns-aliases'.

This is intended to be used to support completion, and shouldn't
be considered an exhaustive list."
  (read (let ((ns (clojure-find-ns)))
          (scrim--redirect-result-from-process
           (scrim-proc)
           (format "(try
  #?(:clj
     (let [ns '%s]
       (concat
        (map str (keys (ns-interns ns)))
        (map str (keys (ns-refers ns)))
        (for [[alias ns'] (ns-aliases ns), sym (keys (ns-publics ns'))] (str alias \"/\" sym))
        (map str (keys (ns-imports ns)))
        (map str (vals (ns-imports ns)))))
     :cljs
     (concat
      (map str (keys (ns-publics 'cljs.core)))
      (map str (keys (ns-interns '%s)))
      (map str (keys (ns-imports '%s)))
      (map str (vals (ns-imports '%s)))))
  (catch #?(:clj Throwable :cljs :default) e
         nil))"
                   ns ns ns ns)))))

(defun scrim--repl-get-namespaced-symbol (symbol)
  "Returns the namespaced symbol for the given symbol in the
current namespace."
  (or (read (scrim--redirect-result-from-process
             (scrim-proc)
             (format "(try (when-let [m (meta (resolve '%s))] (str (:ns m) \"/\" (:name m)))
                       (catch #?(:clj Throwable :cljs :default) e nil))"
                     symbol)))
      (error "Could not resolve symbol.")))

(defun scrim--repl-get-path-to-namespace-source-file (ns)
  "Query the REPL for the path to the source file for namespace
ns.

In cljs, this returns nil, because I don't know how to get the
source file location for a namespace in cljs."
  (read (scrim--redirect-result-from-process
         (scrim-proc)
         (format "#?(:clj
   ;; Based on clojure.core/load-one, clojure.core/load, and clojure.lang.RT/load.
   (let [path (#'clojure.core/root-resource (ns-name '%s))
         path (if (.startsWith path \"/\")
                path
                (str (#'clojure.core/root-directory (ns-name *ns*)) \\/ path))
         path (.substring path 1)]
     (str (or #_(.getResource (clojure.lang.RT/baseLoader) (str path \"__init\" \".class\"))
              (.getResource (clojure.lang.RT/baseLoader) (str path \".clj\"))
              (.getResource (clojure.lang.RT/baseLoader) (str path \".cljc\")))))
   :cljs
   nil)"
                 ns))))

(defun scrim--repl-get-path-to-symbol-source (symbol)
  "Query the REPL for file, line, and column of the symbol's
source definition.

In cljs, this returns nil, because I don't know how to get the
source file location for a symbol in cljs."
  (read (scrim--redirect-result-from-process
         (scrim-proc)
         (format "#?(:clj
   (when-let [{:keys [file line column]} (meta (resolve '%s))]
     (when (not (= \"NO_SOURCE_PATH\" file))
       (list (or (some-> (.getResource (clojure.lang.RT/baseLoader) file) str)
                 (str \"file:\"file))
             line
             column)))
   :cljs
   nil)" symbol))))

(defun scrim--repl-get-possible-references (identifier)
  "Takes a namespaced symbol and returns a list of (file strings),
where file is a file that might include a reference to this
identifier, and strings is a list of possible forms this
reference might take, such as a simple symbol if the symbol is
referred, or an aliased symbol if the symbol's namespace is
aliased.

In cljs, this returns nil, because cljs doesn't have `all-ns' or
`ns-refers'.

This is intended to be used in an implementation of
`xref-backend-references'."
  ;; This has some limitations:

  ;;   `identifier` must be a fully qualified symbol.

  ;;   Doesn't detect references via :use.

  ;;   Doesn't find fully qualified symbols, unless they are also mentioned in
  ;;   the namespace's refers or aliases.

  ;;   Doesn't find symbols evaluated in the REPL.

  (let* ((namespaces (scrim--repl-get-all-namespaces))
         (xs         (read (scrim--redirect-result-from-process
                            (scrim-proc)
                            (format "#?(:clj
   (let [symbol-in     '%s
         symbol-ns     (namespace symbol-in)
         symbol-name   (name symbol-in)
         simple-symbol (symbol symbol-name)]
     (keep (fn [ns]
             (let [strings (concat
                            ;; Source ns
                            (when (= (name (ns-name ns)) symbol-ns)
                              [symbol-name])
                            ;; Referred (make sure ns matches)
                            (when (some->
                                    (ns-refers ns)
                                    (get simple-symbol)
                                    meta
                                    :ns
                                    ns-name
                                    name
                                    (= symbol-ns))
                              [symbol-name])
                            ;; Aliased (make sure ns matches)
                            (for [[k v] (ns-aliases ns)
                                  :when (= (name (ns-name v)) symbol-ns)]
                              (str k \"/\" symbol-name)))]
               (when (seq strings)
                 (list (ns-name ns) strings))))
           (all-ns)))
   :cljs nil)"
                                    identifier)))))
    (mapcar (lambda (x)
              (let* ((ns      (car x))
                     (strings (cadr x))
                     (file    (scrim--repl-get-path-to-namespace-source-file ns)))
                (list file strings)))
            xs)))


;;;; Commands that build common Clojure expressions, usually based on symbols
;;;; near point, and send them to the REPL.

(defcustom scrim-always-prompt-p nil
  "If non-nil, interactive commands that take an argument and send
expressions to the REPL will always prompt the user before sending."
  :type 'boolean
  :safe 'booleanp)

(defmacro scrim--cmd (name docstring default-fn prompt-fn clj-format-string error-msg)
  "Macro for defining simple commands that compose a Clojure
expression, usually based on a symbol near point, and send it to
the REPL process.

NAME is a symbol that will be the name of the new command.

DOCSTRING is a docstring for the command.

DEFAULT-FN and PROMPT-FN are used to produce an input
value. DEFAULT-FN is either nil or a function that takes no
arguments and returns an input value. PROMPT-FN is a function
that takes whatever DEFAULT-FN returns, or nil if DEFAULT-FN is
nil, and returns an input value.

The idea is that DEFAULT-FN is used to automatically guess which
symbol or expression the user probably wants, and PROMPT-FN is
used to prompt the user to either confirm the automatic value or
supply a new one.

PROMPT-FN is only called if DEFAULT-FN is nil, if DEFAULT-FN
returns nil or a blank string, if a prefix argument was supplied,
or if scrim-always-prompt-p is non-nil. This allows users to
control the conditions under which they will get prompted. For
example, one user might want the command to DWIM and avoid a
prompt, if possible, whereas another user might want to confirm
every expression before it is sent to the REPL.

CLJ-FORMAT-STRING is a format string for a Clojure expression. It
should take one argument: the input value.

ERROR-MSG will be displayed if the input value is nil or a blank
string."
  `(defun ,name (arg)
     ,docstring
     (interactive (let ((arg (when ,default-fn (funcall ,default-fn))))
                    (if (or current-prefix-arg
                            scrim-always-prompt-p
                            (null arg)
                            (and (stringp arg)
                                 (string-blank-p arg)))
                        (list (funcall ,prompt-fn arg))
                      (list arg)))
                  scrim-mode scrim-minor-mode)
     (if (or (null arg)
             (string-blank-p arg))
         (user-error ,error-msg)
       (scrim--send (scrim-proc) (format ,clj-format-string arg)))))

;;; core

(scrim--cmd scrim-send-require
            "Send (require ns) to the REPL."
            'clojure-find-ns
            (lambda (default-ns)
              (read-string (format-prompt "require ns" "default-ns")
                           nil nil default-ns))
            "(require '%s)"
            "Namespace not found")

(scrim--cmd scrim-send-in-ns
            "Send (in-ns ns) to the REPL."
            'clojure-find-ns
            (lambda (default-ns)
              (completing-read (format-prompt "in ns" default-ns)
                               (completion-table-with-cache
                                (lambda (s)
                                  (scrim--repl-get-all-namespaces)))
                               nil nil nil nil
                               default-ns))
            "(in-ns '%s)"
            "Namespace not found")

(scrim--cmd scrim-send-arglists
            "Send (:arglists (meta (resolve ns))) to the REPL."
            'scrim-current-function-symbol
            (lambda (default-symbol)
              (completing-read (format-prompt "arglists for function" default-symbol)
                               (completion-table-dynamic
                                (lambda (s)
                                  (append (scrim--repl-get-all-symbols-in-current-ns)
                                          (scrim--repl-get-all-namespaced-symbols)))
                                t)
                               nil nil nil nil
                               default-symbol))
            "(:arglists (meta (resolve '%s)))"
            "No function near point")

(scrim--cmd scrim-send-macroexpand
            "Send (macroexpand form) to the REPL."
            'scrim-last-sexp
            (lambda (form)
              (read-string (format-prompt "macroexpand form" form)
                           nil nil form))
            "(macroexpand '%s)"
            "No sexp found")

(scrim--cmd scrim-send-macroexpand-1
            "Send (macroexpand-1 form) to the REPL."
            'scrim-last-sexp
            (lambda (form)
              (read-string (format-prompt "macroexpand-1 form" form)
                           nil nil form))
            "(macroexpand-1 '%s)"
            "No sexp found")

(scrim--cmd scrim-send-macroexpand-all
            "Send (clojure.walk/macroexpand-all form) to the REPL."
            'scrim-last-sexp
            (lambda (form)
              (read-string (format-prompt "macroexpand-all form" form)
                           nil nil form))
            "(clojure.walk/macroexpand-all '%s)"
            "No sexp found")

(scrim--cmd scrim-send-load-file
            "Send (load-file name) to the REPL."
            (lambda () buffer-file-name)
            (lambda (default-file-name)
              (let ((file (expand-file-name
                           (read-file-name "file: "
                                           nil default-file-name t
                                           (file-name-nondirectory default-file-name)))))
                (comint-check-source file)
                file))
            "(load-file \"%s\")"
            "No file found")

;;; repl

;; TODO: Can't cache this completion because we need the SWITCH-BUFFER
;; argument that only completion-table-dynamic provides, in order to
;; use `clojure-find-ns' in `scrim--repl-get-all-symbols-in-current-ns'.
(scrim--cmd scrim-send-doc
            "Send (clojure.repl/doc name) to the REPL."
            'scrim-symbol-at-point
            (lambda (default-symbol)
              (completing-read (format-prompt "name" default-symbol)
                               (completion-table-dynamic
                                (lambda (s)
                                  ;; TODO: Include keywords, for specs.
                                  (append (scrim--repl-get-all-namespaces)
                                          (scrim--repl-get-all-symbols-in-current-ns)
                                          (scrim--repl-get-all-namespaced-symbols)))
                                t)
                               nil nil nil nil
                               default-symbol))
            "(clojure.repl/doc %s)"
            "No name near point")

(scrim--cmd scrim-send-find-doc
            "Send (clojure.repl/find-doc re-string-or-pattern) to the REPL."
            nil
            (lambda (x) (read-string (format-prompt "re-string-or-pattern" nil)))
            "(clojure.repl/find-doc %s)"
            "No input")

(scrim--cmd scrim-send-source
            "Send (clojure.repl/source n) to the REPL."
            'scrim-symbol-at-point
            (lambda (default-symbol)
              (completing-read (format-prompt "symbol" default-symbol)
                               (completion-table-dynamic
                                (lambda (s)
                                  (append (scrim--repl-get-all-namespaces)
                                          (scrim--repl-get-all-symbols-in-current-ns)
                                          (scrim--repl-get-all-namespaced-symbols)))
                                t)
                               nil nil nil nil
                               default-symbol))
            "(clojure.repl/source %s)"
            "No symbol near point")

(scrim--cmd scrim-send-dir
            "Send (clojure.repl/dir nsname) to the REPL."
            'clojure-find-ns
            (lambda (default-ns)
              (completing-read (format-prompt "ns" default-ns)
                               (completion-table-with-cache
                                (lambda (s)
                                  (scrim--repl-get-all-namespaces)))
                               nil nil nil nil
                               default-ns))
            "(clojure.repl/dir %s)"
            "No namespace found")

(scrim--cmd scrim-send-apropos
            "Send (doseq [v (sort (clojure.repl/apropos str-or-pattern))] (println v)) to the REPL."
            nil
            (lambda (x) (read-string (format-prompt "str-or-pattern" nil)))
            "(doseq [v (sort (clojure.repl/apropos %s))] (println v))"
            "No input")

(defun scrim-send-pst ()
  "Send (clojure.repl/pst) to the REPL."
  (interactive nil scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) "(clojure.repl/pst)"))

;;; pretty print

(defun scrim-send-pp ()
  "Send #?(:clj (clojure.pprint/pp) :cljs (cljs.pprint/pp)) to the REPL."
  (interactive nil scrim-mode scrim-minor-mode)
  (scrim--send (scrim-proc) "#?(:clj (clojure.pprint/pp) :cljs (cljs.pprint/pp))"))

;;; javadoc

(scrim--cmd scrim-send-javadoc
            "Send (clojure.java.javadoc/javadoc class-or-object) to the REPL."
            nil
            (lambda (x) (read-string (format-prompt "class-or-object" nil)))
            "(clojure.java.javadoc/javadoc %s)"
            "No input")


;;;; Keymaps

(defvar scrim-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-q") #'scrim-quit)
    (define-key map (kbd "C-c o")   #'scrim-clear-repl-buffer)
    (define-key map (kbd "C-c C-s e") #'scrim-repl-buffer-end)
    (define-key map (kbd "C-c C-z")   #'scrim-show-or-hide-repl-buffer)
    map))

(defvar scrim-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-M-c") #'scrim-connect)

    (define-key map (kbd "C-c C-q")   #'scrim-quit)

    (define-key map (kbd "C-c C-z")   #'scrim-show-or-hide-repl-buffer)

    (define-key map (kbd "C-c o")     #'scrim-clear-repl-buffer)
    (define-key map (kbd "C-c C-o")   #'scrim-clear-repl-buffer)

    (define-key map (kbd "C-c C-s e") #'scrim-repl-buffer-end)

    (define-key map (kbd "C-c e")     #'scrim-eval-last-sexp)
    (define-key map (kbd "C-c C-e")   #'scrim-eval-last-sexp)
    (define-key map (kbd "C-x C-e")   #'scrim-eval-last-sexp)

    (define-key map (kbd "C-c C-c")   #'scrim-eval-defn)
    (define-key map (kbd "C-M-x")     #'scrim-eval-defn)

    (define-key map (kbd "C-c b")     #'scrim-eval-buffer)
    (define-key map (kbd "C-c C-b")   #'scrim-eval-buffer)

    (define-key map (kbd "C-c C-M-r") #'scrim-eval-region)

    (define-key map (kbd "C-c l")     #'scrim-send-load-file)
    (define-key map (kbd "C-c C-l")   #'scrim-send-load-file)

    (define-key map (kbd "C-c r")     #'scrim-send-require)
    (define-key map (kbd "C-c C-r")   #'scrim-send-require)

    (define-key map (kbd "C-c n")     #'scrim-send-in-ns)
    (define-key map (kbd "C-c C-n")   #'scrim-send-in-ns)

    (define-key map (kbd "C-c a")     #'scrim-send-arglists)
    (define-key map (kbd "C-c C-a")   #'scrim-send-arglists)

    (define-key map (kbd "C-c m m")   #'scrim-send-macroexpand)
    (define-key map (kbd "C-c C-m m") #'scrim-send-macroexpand)

    (define-key map (kbd "C-c m 1")   #'scrim-send-macroexpand-1)
    (define-key map (kbd "C-c C-m 1") #'scrim-send-macroexpand-1)

    (define-key map (kbd "C-c m a")   #'scrim-send-macroexpand-all)
    (define-key map (kbd "C-c C-m a") #'scrim-send-macroexpand-all)

    (define-key map (kbd "C-c C-d d") #'scrim-send-doc)
    (define-key map (kbd "C-c C-d j") #'scrim-send-javadoc)
    (define-key map (kbd "C-c C-d f") #'scrim-send-find-doc)
    (define-key map (kbd "C-c C-d s") #'scrim-send-source)
    (define-key map (kbd "C-c C-d a") #'scrim-send-apropos)

    (define-key map (kbd "C-c C-M-d") #'scrim-send-dir)

    (define-key map (kbd "C-c C-M-e") #'scrim-send-pst)
    (define-key map (kbd "C-c p")     #'scrim-send-pp)

    map))


;;;; Modes

;;;###autoload
(define-minor-mode scrim-minor-mode
  "Minor mode for interacting with the Scrim REPL buffer.

\\{scrim-minor-mode-map}"
  :lighter " Scrim"
  :keymap scrim-minor-mode-map)

(define-derived-mode scrim-mode comint-mode "scrim"
  "Major mode for a Clojure REPL.

\\{scrim-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (setq-local comint-prompt-regexp scrim-prompt-regexp)
  (setq-local comint-scroll-to-bottom-on-input t)
  (setq-local mode-line-process '(":%s"))
  (setq-local comint-prompt-read-only scrim-prompt-read-only)
  ;; Keep original text properties.
  (setq-local comint-highlight-input nil)
  (setq-local indent-line-function #'scrim--indent-line)
  (setq-local help-at-pt-display-when-idle t)
  (add-to-invisibility-spec '(scrim . t))
  ;;(ansi-color-for-comint-mode-on)
  (add-hook 'comint-preoutput-filter-functions #'scrim-preoutput-filter nil t)
  (font-lock-mode 0))


;;;; Starting

;;;###autoload
(defun scrim (program &rest args)
  "Launch a Scrim REPL buffer, running PROGRAM.

PROGRAM should be one of the following:
- a string, denoting an executable program that launches a
  Clojure REPL
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to a Clojure socket server

Note that PROGRAM must be something that launches a Clojure
native REPL, like \"clojure\" or \"clj\" from the Clojure CLI
tools. \"lein repl\" will not work, for instance, because it uses
nrepl, which this library does not support. A workaround is to
launch a process with a socket server, outside of Emacs, and
connect to it via `scrim-connect'."
  (interactive (cons (read-string (format-prompt "program" "clojure") nil nil "clojure")
                     (split-string (read-string (format-prompt "args" nil) nil nil ""))))
  (if (get-buffer-process scrim--buffer-name)
      (user-error "Already connected.")
    (message "Starting a Clojure REPL...")
    (let ((default-directory (scrim-project-root))
          ;; Binding process-connection-type to nil causes the communication with
          ;; the subprocess to use a pipe rather than a pty. Without this,
          ;; expressions longer than 1024 bytes cannot be sent to the subprocess.
          (process-connection-type nil))
      (display-buffer (get-buffer-create scrim--buffer-name))
      (apply #'make-comint-in-buffer "scrim" scrim--buffer-name program nil args)
      (save-excursion
        (set-buffer scrim--buffer-name)
        (scrim-mode)))
    (message "Starting a Clojure REPL...done")))

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
  "Connect to a Clojure REPL socket server.

Same as (scrim '(host . port)).

For example, a Clojure process with a socket server can be
created (outside of Emacs) with one of the following commands:

Just Clojure:
java -Dclojure.server.repl='{:port 5555 :accept clojure.core.server/repl}' -jar path-to-clojure-jar

Leiningen:
JVM_OPTS='-Dclojure.server.myrepl={:port,5555,:accept,clojure.core.server/repl}' lein repl

CLI tools:
clj -J-Dclojure.server.myrepl='{:port 5555,:accept,clojure.core.server/repl}'

Then connect (in Emacs) with:

m-x scrim-connect RET localhost RET 5555 RET"
  (interactive (list (read-string (format-prompt "host" scrim-default-host)
                                  nil nil scrim-default-host)
                     (read-number "port: "
                                  scrim-default-port)))
  (scrim (cons host port)))

(provide 'scrim)

;;; scrim.el ends here
