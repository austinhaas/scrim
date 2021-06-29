
(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/clojure-mode/")
(require 'clojure-mode)

(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/scrim/")
(require 'scrim)

(setq clojure-refactor-map-prefix nil) ;; Conflicts with scrim keybinding for "C-c C-r".

(add-hook 'clojure-mode-hook #'scrim-minor-mode)

(add-hook 'scrim-mode-hook #'clojure-mode-variables)
(add-hook 'scrim-mode-hook #'clojure-font-lock-setup)
