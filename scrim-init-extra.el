
(customize-set-variable 'scrim-default-host "localhost") ;; This is the default.
(customize-set-variable 'scrim-default-port 5555) ;; This is the default.

(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/paredit.git")

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(require 'rainbow-delimiters)

(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'hs-minor-mode)

(add-hook 'scrim-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'scrim-mode-hook 'enable-paredit-mode) ;; Not working, because it binds C-d, which scrim-mode uses to quit.

;; Add the hideshow minor mode to the scrim REPL buffer.
;; This has to run after clojure-mode-variables, hence the 'append argument.
(add-hook 'scrim-mode-hook #'hs-minor-mode 'append)

(defun my-scrim-hide-last-input ()
  "Collapse the last input in the REPL to a single line, using
hideshow. You can expand it using the normal hideshow commands."
  (save-excursion
    (with-current-buffer scrim--buffer-name
      (goto-char comint-last-input-end)
      (backward-sexp)
      (forward-char)
      (hs-hide-block))))

(defun my-scrim-echo-output ()
  "Display the last output in the echo area."
  (message "%s" (scrim-last-output)))

(defun my-scrim-output-filter (s)
  "A function to run each time the scrim REPL buffer receives
output from the Java process."

  (when (not (string-equal s "user=> "))

    ;; There may be a better place for this.
    (my-scrim-hide-last-input)

    ;; This function may be called multiple times; each call containing a portion
    ;; of the complete output. For example, after receiving the user's input, this
    ;; function may be called with a blank string, then again with the result, and
    ;; then again with a prompt. The result value may also be split across
    ;; multiple calls. In order to display the complete result, the value is read
    ;; from the REPL buffer, instead of using the string argument to this
    ;; function.
    (my-scrim-echo-output)))

(defun init-scrim-mode ()
  "My customizations."
  (add-hook 'comint-output-filter-functions #'my-scrim-output-filter nil t))

(add-hook 'scrim-mode-hook #'init-scrim-mode)
