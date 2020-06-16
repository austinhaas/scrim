(require 'ansi-color)
(require 'cl-lib)
(require 'clojure-mode)
(require 'comint)
(require 'subr-x)
(require 'thingatpt)


(defconst scrim-version "0.0.1"
  "The current version of `scrim'.")

;;;; Utility

(defun scrim--prompt (prompt default)
  "Prompt user for input. If default is not nil, it will be
included in the prompt and returned as the value if the user
enters a blank string."
  (let* ((prompt (if default
                     (format "%s [%s]: " prompt default)
                   (format "%s: " prompt)))
         (ans    (read-string prompt)))
    (if (string-blank-p ans)
        default
      ans)))

;;;; Functions to extract values from Clojure buffers

(defun scrim-symbol-at-point ()
  (thing-at-point 'symbol t))

(defun scrim-sexp-at-point ()
  (or (thing-at-point 'symbol t)
      (thing-at-point 'sexp t)))

;; Add a custom symbol for recognizing the "outermost" sexp from point. The behavior associated with
;; 'defun does almost what we want, but it includes a trailing newline, and we don't want that.
(put 'scrim-outermost-sexp 'bounds-of-thing-at-point
     (lambda ()
       (save-excursion
         (let ((start (point)))
           (end-of-defun)
           (beginning-of-defun)
           (let ((beginning (point)))
             (forward-sexp)
             (let ((end (point)))
               (when (<= beginning start end)
                 (cons beginning end))))))))

(defun scrim-outermost-sexp-at-point ()
  (thing-at-point 'scrim-outermost-sexp t))

(defun scrim-sexps-in-region (start end)
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

(defun scrim-outermost-sexp-function-symbol ()
  (condition-case nil
      (save-excursion
        (beginning-of-thing 'scrim-outermost-sexp)
        (forward-thing 'symbol)
        (thing-at-point 'symbol t))
    (error nil)))

(defun scrim-ns-at-point ()
  ;; TODO: Remove dependency on clojure-mode.
  (clojure-find-ns))

;;;; Configuration

(defgroup scrim nil
  "scrim group"
  :prefix "scrim-"
  :group 'clojure
  :link '(url-link :tag "GitHub" "https://github.com/"))

(defcustom scrim-prompt-read-only t
  "If t, the prompt in the scrim REPL buffer is read-only."
  :type 'boolean)

(defcustom scrim-prompt-regexp "^[^=> \n]+=> *"
  "Regexp for the Clojure prompt. Default should match the
default Clojure REPL prompt.
See https://clojure.github.io/clojure/clojure.main-api.html#clojure.main/repl
for customizing that prompt."
  :type 'regexp)

;;;; REPL buffer

(defvar scrim--buffer-name "*scrim*"
  "The name of the scrim REPL buffer.")

(defun scrim-proc ()
  "Return the current scrim REPL process, or nil if it doesn't
exist."
  (get-buffer-process scrim--buffer-name))

(defun scrim-clear-repl-buffer ()
  "Clear the scrim REPL buffer."
  (interactive)
  (comint-clear-buffer))

(defun scrim-repl-buffer-end ()
  (interactive)
  (if (get-buffer scrim--buffer-name)
    (set-window-point (get-buffer-window scrim--buffer-name "visible")
                      (process-mark (scrim-proc)))
    (user-error "Not connected.")))

(defun scrim-show-repl-buffer ()
  "Show the scrim REPL buffer, if it exists and is not already
visible."
  (interactive)
  (if (get-buffer scrim--buffer-name)
      ;; Unless it is already visible.
      (unless (get-buffer-window scrim--buffer-name "visible")
        (display-buffer scrim--buffer-name))
    (user-error "Not connected.")))


;;;; Low-level, comint I/O

(defcustom scrim-echo-input-p t
  "If t, first send input to the REPL buffer, then send it to the
process from there. This has the effect of capturing the complete
interaction history in the REPL buffer. That means that the input
is visible, interleaved with the output, and also inputs are
recorded and can be recalled with commands like
comint-previous-input, which is typically bound to several keys
in the REPL buffer.

  If nil, send input directly to the REPL process."
  :type 'boolean)

(defcustom scrim-echo-output-p t
  "If t, echo output in the echo area."
  :type 'boolean)

(defun scrim--send (proc s)
  (if scrim-echo-input-p
      (with-current-buffer scrim--buffer-name
        (save-excursion
          (comint-goto-process-mark)
          (insert s)
          (comint-send-input)))
    (comint-simple-send proc s)))

(defun scrim--echo-output (s)
  "Display output in the echo area."
  ;; Remove trailing prompt, if present.
  (let* ((s (replace-regexp-in-string (concat scrim-prompt-regexp "\\'") "" s))
         ;; Remove trailing newlines, if present.
         (s (replace-regexp-in-string "\n+\\'" "" s)))
    (unless (string-match-p scrim-prompt-regexp s)
      (message s))))

(defun scrim--preoutput-filter (s)
  (when scrim-echo-output-p
    (scrim--echo-output s))
  s)


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

(defun scrim-eval-outermost-sexp-at-point ()
  (interactive)
  (let ((s (scrim-outermost-sexp-at-point)))
    (if s
        (scrim--send (scrim-proc) s)
      (user-error "No expression."))))

(defun scrim-eval-innermost-sexp-at-point ()
  "Send the expression nearest to point to the REPL
process."
  (interactive)
  (let ((s (scrim-sexp-at-point)))
    (if s
        (scrim--send (scrim-proc) s)
      (user-error "No expression."))))

(defun scrim-quit ()
  "Send EOF to the scrim REPL process."
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

    (define-key map (kbd "C-c C-i")   #'scrim-eval-innermost-sexp-at-point)
    (define-key map (kbd "C-c C-o")   #'scrim-eval-outermost-sexp-at-point)

    (define-key map (kbd "C-c C-r")   'scrim-repl-map)
    (define-key map (kbd "C-c C-p")   'scrim-pprint-map)
    map))

(defvar scrim-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-S-c") #'scrim-connect)
    (define-key map (kbd "C-c C-q")   #'scrim-quit)
    (define-key map (kbd "C-c C-S-o") #'scrim-clear-repl-buffer)

    (define-key map (kbd "C-c C-z")   #'scrim-show-repl-buffer)
    (define-key map (kbd "C-c C-S-e") #'scrim-repl-buffer-end)

    (define-key map (kbd "C-c C-i")   #'scrim-eval-innermost-sexp-at-point)
    (define-key map (kbd "C-c C-o")   #'scrim-eval-outermost-sexp-at-point)
    (define-key map (kbd "C-c C-b")   #'scrim-eval-buffer)
    (define-key map (kbd "C-c C-S-r") #'scrim-eval-region)

    (define-key map (kbd "C-c C-n")   #'scrim-send-in-ns)
    (define-key map (kbd "C-c C-a")   #'scrim-send-arglists)
    (define-key map (kbd "C-c C-m")   #'scrim-send-macroexpand)

    (define-key map (kbd "C-c C-r")   'scrim-repl-map)
    (define-key map (kbd "C-c C-p")   'scrim-pprint-map)
    map))


;;;; Modes

;;;###autoload
(define-minor-mode scrim-minor-mode
  "Minor mode for interacting with the scrim REPL buffer.

Commands:

\\{scrim-minor-mode-map}"
  :lighter " scrim"
  :keymap scrim-minor-mode-map
  (setq-local comint-input-sender 'scrim--send))

(define-derived-mode scrim-mode comint-mode "scrim"
  (setq comint-prompt-regexp scrim-prompt-regexp)
  (setq mode-line-process '(":%s"))
  (add-hook 'comint-preoutput-filter-functions #'scrim--preoutput-filter nil t)
  (setq-local comint-prompt-read-only scrim-prompt-read-only)
  (ansi-color-for-comint-mode-on))


;;;; Starting

;;;###autoload
(defun scrim (program)
  "Launch a scrim REPL buffer, running PROGRAM.

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


;;;; Commands that send common expressions to the REPL.

;;; core

(defun scrim-send-in-ns (prompt)
  (interactive "P")
  (let ((name (if prompt
                  (scrim--prompt "Set namespace to symbol" (scrim-ns-at-point))
                (scrim-ns-at-point))))
    (if name
        (scrim--send (scrim-proc) (format "(in-ns '%s)" name))
      (user-error "No namespace found"))))

(defun scrim-send-macroexpand (&optional macro-1)
  (interactive "P")
  (let ((expr (scrim-outermost-sexp-at-point)))
    (if expr
        (scrim--send (scrim-proc) (format (if macro-1
                                              "(macroexpand-1 '%s)"
                                            "(macroexpand '%s)")
                                          expr))
      (user-error "No macro near point"))))

(defun scrim-send-arglists (prompt)
  (interactive "P")
  (let ((fn (if prompt
                (scrim--prompt "arglists for fn" (scrim-outermost-sexp-function-symbol))
              (scrim-outermost-sexp-function-symbol))))
    (if fn
        (scrim--send (scrim-proc) (format "(:arglists (meta (resolve '%s)))" fn))
      (user-error "No function near point"))))

;;; repl

(defun scrim-send-doc (prompt)
  (interactive "P")
  (let ((name (if prompt
                 (scrim--prompt "doc for name" (scrim-symbol-at-point))
                (scrim-symbol-at-point))))
    (if name
        (scrim--send (scrim-proc) (format "(clojure.repl/doc %s)" name))
      (user-error "No name found"))))

(defun scrim-send-source (prompt)
  (interactive "P")
  (let ((n (if prompt
               (scrim--prompt "source for symbol" (scrim-symbol-at-point))
             (scrim-symbol-at-point))))
    (if n
        (scrim--send (scrim-proc) (format "(clojure.repl/source %s)" n))
      (user-error "No symbol found"))))

(defun scrim-send-dir (prompt)
  (interactive "P")
  (let ((nsname (if prompt
                    (scrim--prompt "dir for nsname" (scrim-ns-at-point))
                  (scrim-ns-at-point))))
    (if nsname
        (scrim--send (scrim-proc) (format "(clojure.repl/dir %s)" nsname))
      (user-error "No nsname found"))))

(defun scrim-send-apropos (prompt)
  (interactive "P")
  (let ((str-or-pattern (if prompt
                            (scrim--prompt "apropos for symbol" (scrim-symbol-at-point))
                          (scrim-symbol-at-point))))
    (if str-or-pattern
        (scrim--send (scrim-proc) (format "(doseq [v (sort (clojure.repl/apropos \"%s\"))] (println v))"
                                          str-or-pattern))
      (user-error "No str-or-pattern found"))))

;;; pretty print

(defun scrim-send-pp ()
  (interactive)
  (scrim--send (scrim-proc) "(clojure.pprint/pp)"))

(provide 'scrim)

;;; scrim.el ends here
