(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/clojure-mode/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/scrim/")
(require 'scrim)
(add-hook 'clojure-mode-hook #'scrim-minor-mode)
