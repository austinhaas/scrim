;; Remove some clojure-mode keybindings that I don't use.
(define-key clojure-mode-map (kbd "C-:") nil)
(define-key clojure-mode-map clojure-refactor-map-prefix nil)

;; Customizable variables (These are set to the defaults; just listing them here
;; and showing how to change.)
(customize-set-variable 'scrim-default-host "localhost")
(customize-set-variable 'scrim-default-port 5555)
(customize-set-variable 'scrim-always-prompt-p nil)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/paredit.git")
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code."
;;   t)
;; (add-hook 'scrim-mode-hook 'enable-paredit-mode) ;; Not working, because it binds C-d, which scrim-mode uses to quit.

(defun my-scrim-output-filter (s)
  "A function to run each time the scrim REPL buffer receives
output from the Java process."
  ;; This function may be called multiple times; each call containing a portion
  ;; of the complete output. For example, after receiving the user's input, this
  ;; function may be called with a blank string, then again with the result, and
  ;; then again with a prompt. The result value may also be split across
  ;; multiple calls. In order to display the complete result, the value is read
  ;; from the REPL buffer, instead of using the string argument to this
  ;; function.

  ;; Show output in the minibuffer
  (when (string-match scrim-prompt-regexp s) ;; Wait for the prompt.
    (message "%s" (scrim-last-output))))

;; Adds support for constructing and querying a database of metadata
;; in the running Clojure process. This can be used for eldoc, for
;; example.
(load-file "~/.emacs.d/site-lisp/third-party/scrim/scrim-db.el")

(defun init-scrim-mode ()
  "Scrim major mode customizations."

  ;; Call `my-scrim-output-filter' whenever the REPL buffer receives
  ;; input from the connected Clojure process.
  (add-hook 'comint-output-filter-functions #'my-scrim-output-filter nil t)

  ;; In the REPL buffer (i.e., where the scrim major mode is
  ;; activated), eldoc currently works for symbols in clojure.core and
  ;; fully-qualified symbols, but nothing else because it doesn't know
  ;; which ns it is in.  Even if we knew the current ns, the repl
  ;; buffer has input from many namespaces that might not be current.
  (setq-local eldoc-documentation-function 'scrim--db-eldoc-function)
  )

(add-hook 'scrim-mode-hook #'init-scrim-mode)

(defun scrim--completion-at-point ()
  (let* ((sym (scrim-symbol-at-point))
         (start (beginning-of-thing 'symbol))
         (end (end-of-thing 'symbol))
         (collection (completion-table-with-cache (lambda (s)
                                                    (append (scrim--repl-get-all-namespaces)
                                                            (scrim--repl-get-all-symbols-in-current-ns)
                                                            (scrim--repl-get-all-namespaced-symbols)))))
         (props nil))
    (cons start (cons end (cons collection props)))))

;; Adds support for xref, which includes features like jumping to a
;; symbol's source definition.
(load-file "~/.emacs.d/site-lisp/third-party/scrim/scrim-xref.el")

(defun init-scrim-minor-mode ()
  "Scrim minor mode customizations."
  (setq-local eldoc-documentation-function 'scrim--db-eldoc-function)
  (add-hook 'completion-at-point-functions #'scrim--completion-at-point nil t)
  (add-hook 'xref-backend-functions #'scrim--xref-backend nil t)
  )

(add-hook 'scrim-minor-mode-hook #'init-scrim-minor-mode)
