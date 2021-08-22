
(customize-set-variable 'scrim-default-host "localhost") ;; This is the default.
(customize-set-variable 'scrim-default-port 5555) ;; This is the default.

(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/paredit.git")

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(require 'rainbow-delimiters)

(add-hook 'scrim-mode-hook #'rainbow-delimiters-mode)
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
  (when (string-match scrim-prompt-regexp s) ;; Wait for the prompt.
    (message "%s" (scrim-last-output))))

(defun init-scrim-mode ()
  "My major mode customizations."
  (add-hook 'comint-output-filter-functions #'my-scrim-output-filter nil t))

(add-hook 'scrim-mode-hook #'init-scrim-mode)

(defun my-scrim-project-find (dir)
  "Try to determine the project root by searching for both a vc dir
and a clojure project build file. If the results are the same,
then return the vc implementation, since it filters files better
by honoring any vc ignore files. Otherwise, return the version
based on clojure project build files, since that is a more
accurate indicator of the project root."
  (let ((vc (project-try-vc dir))
        (clj (scrim-clojure-project-find dir)))
    (if (and vc clj)
        (if (string-equal (project-root vc)
                          (project-root clj))
            vc
          clj)
      (or vc clj))))

(defun init-scrim-minor-mode ()
  "My minor mode customizations."
  (add-hook 'project-find-functions #'my-scrim-project-find nil t)
  ;; Alternatively, you could add the following expression instead, which would
  ;; try to find the clojure build file first (since it is added to the local
  ;; hook), and then fallback to searching for a vc dir (which is on the global
  ;; hook). If you want to change that priority, then you would need to add
  ;; `project-try-vc' to the local hook here, too, so that you can prioritize
  ;; them.
  ;; (add-hook 'project-find-functions #'scrim-clojure-project-find nil t)
  )

(add-hook 'scrim-minor-mode-hook #'init-scrim-minor-mode)
