(require 'ansi-color)
(require 'cl-lib)
(require 'clojure-mode)
(require 'comint)
(require 'subr-x)
(require 'thingatpt)
(require 'xref)


(defconst scrim-version "0.0.4"
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
    (message "pos: %s" pos)
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
    (comint-clear-buffer)))

(defun scrim-repl-buffer-end ()
  (interactive)
  (if (get-buffer scrim--buffer-name)
      (set-window-point (get-buffer-window scrim--buffer-name "visible")
                        (process-mark (scrim-proc)))
    (user-error "Not connected.")))

(defun scrim-show-repl-buffer ()
  "Show the Scrim REPL buffer, if it exists and is not already
visible."
  (interactive)
  (if (get-buffer scrim--buffer-name)
      ;; Unless it is already visible.
      (unless (get-buffer-window scrim--buffer-name "visible")
        (display-buffer scrim--buffer-name))
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

(defun scrim--send-indirectly-prepl (proc s)
  (with-current-buffer scrim--buffer-name

    (let ((start (point))
          (end?  (= (point) (point-max))))
      (comint-goto-process-mark)
      (insert s)
      (comint-send-input)
      (unless end? (goto-char start)))))

;; Not used
(defun scrim--send-directly (proc s)
  "Sends the string s to process proc directly."
  (comint-simple-send proc s))

(defun scrim--send (proc s)
  (scrim--send-indirectly-prepl proc s))

(defun scrim--parse-prepl-result (s)
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
		              (accept-process-output process)))
      ;; Collect the output
      (set-buffer output-buffer)
      (let* ((s (buffer-substring-no-properties (point-min) (point-max)))
             ;; Assuming one result!
             (plist (car (scrim--parse-prepl-results s)))
             (val (plist-get plist :val)))
        val))))

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
  (let ((xs (scrim--parse-prepl-results s)))
    (concat
     (propertize s 'face 'scrim-prepl-raw-face)
     (string-join
      (mapcar (lambda (plist)
                (let* ((tag (plist-get plist :tag))
                       (val (plist-get plist :val)))
                  (cond
                   ((equal tag :ret) (if (equal 'true (plist-get plist :exception))
                                         (propertize val 'face 'scrim-prepl-exception-face)
                                       (propertize val 'face 'scrim-prepl-ret-face)))
                   ((equal tag :out) (propertize val 'face 'scrim-prepl-out-face))
                   ((equal tag :err) (propertize val 'face 'scrim-prepl-err-face))
                   ((equal tag :tap) (propertize val 'face 'scrim-prepl-tap-face)))))
              xs)
      "\n")
     "\n"
     (propertize"[user]\n" 'face 'scrim-prepl-prompt-face))))


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
    (define-key map (kbd "M-.")       #'scrim-find-definition)
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

    (define-key map (kbd "M-.")       #'scrim-find-definition)

    (define-key map (kbd "C-c C-z")   #'scrim-show-repl-buffer)
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
  (setq-local comint-input-sender 'scrim--send))

(define-derived-mode scrim-mode comint-mode "scrim"
  "Major mode for a Clojure REPL.

\\{scrim-mode-map}"
  (setq comint-prompt-regexp scrim-prompt-regexp)
  (setq mode-line-process '(":%s"))
  (setq-local comint-prompt-read-only scrim-prompt-read-only)
  ;;(ansi-color-for-comint-mode-on)
  (add-hook 'comint-preoutput-filter-functions #'scrim-preoutput-filter nil t)
  (font-lock-mode 0))


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

;;;###autoload
(defun scrim-connect (host port)
  "Same as (scrim '(host . port))."
  (interactive (list (scrim--prompt "host" "localhost")
                     (scrim--prompt "port" 5555)))
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

(defun scrim--find-file (url)
  (require 'arc-mode)
  (cond ((string-match "^file:\\(.*\\):\\(.*\\)" result)
         (let ((file (match-string 1 result))
               (line (string-to-number (match-string 2 result))))
           (xref-push-marker-stack)
           (find-file file)
           (goto-line line)))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\):\\(.*\\)" url)
         (when-let* ((archive (match-string 2 url))
                     (file    (match-string 3 url))
                     (line    (string-to-number (match-string 4 url)))
                     (name    (format "%s:%s" archive file)))
           (cond
            ((find-buffer-visiting name)
             (xref-push-marker-stack)
             (switch-to-buffer (find-buffer-visiting name))
             (goto-line line))
            (t
             (xref-push-marker-stack)
             (with-current-buffer (generate-new-buffer
                                   (file-name-nondirectory file))
               (archive-zip-extract archive file)
               (set-visited-file-name name)
               (setq-local default-directory (file-name-directory archive))
               (setq-local buffer-read-only t)
               (set-buffer-modified-p nil)
               (set-auto-mode)
               (switch-to-buffer (current-buffer))
               (goto-line line))))))
        (t
         nil)))

(defun scrim-find-definition (prompt)
  (interactive "P")
  (let ((arg (if prompt
                 (scrim--prompt "path to source for symbol" (scrim-symbol-at-point))
               (scrim-symbol-at-point))))
    (if arg
        (let* ((clj "(let [{:keys [file line]} (meta (resolve '%s))]
 (cond
   (nil? file)                 nil
   (= file \"NO_SOURCE_PATH\") nil
   :else                       (str (.getResource (clojure.lang.RT/baseLoader) file) \":\" line)))")
               (result (read (scrim-redirect-result-from-process (scrim-proc) (format clj arg)))))
          (if (null result)
              (error "Couldn't find definition for %s. (Was it evaluated in the REPL?)" arg)
            (scrim--find-file result)))
      (user-error "No symbol near point"))))

(defun scrim-send-pst ()
  (interactive)
  (scrim--send (scrim-proc) "(clojure.repl/pst)"))

;;; pretty print

(defun scrim-send-pp ()
  (interactive)
  (scrim--send (scrim-proc) "(clojure.pprint/pp)"))

(provide 'scrim)

;;; scrim.el ends here
