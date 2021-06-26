# Scrim - Simple Clojure REPL Interaction Mode

An Emacs package for Clojure REPL interaction.

A major mode for connection to a Clojure REPL process in a buffer.

A minor mode for buffers containing Clojure(Script) code.

## Status

Alpha

## Goals

* The basic features necessary to use Emacs as a Clojure REPL interface.
* Compatibility with the REPLs that are included with Clojure.
* Compatibility with Clojure and Clojurescript.
* Strictly minimal, by default.
* Customizable, where appropriate.
* Extensible.
* No magic; never guess what the user wants.

## Non-Goals

* Support for other REPLs.
* Support for multiple REPLs.

  This may be added later, but only if there is a simple, magic-free way to do it, that doesn't complicate the more common case of a single REPL. The main challenge seems to be keeping track of which buffer maps to which REPL.

* Anything that isn't essential to basic REPL interaction, but could be added separately (e.g., eldoc).

  Additional features may be included in a separate, optional, non-essential file. Support (e.g., hooks) may be included in the core file if it can be done simply.

## Dependencies

* [clojure-mode](https://github.com/clojure-emacs/clojure-mode/)

## Installation

### Clone the repository

```sh
cd ~/.emacs.d/site-lisp  # or wherever you keep locally installed emacs packages
git clone git@github.com:austinhaas/scrim.git
```
### Update your .emacs (customize to your preference)

```
;;;; scrim

(add-to-list 'load-path "~/.emacs.d/site-lisp/third-party/scrim/")
(require 'scrim)
(require 'scrim-eldoc) ;; Optional. Eldoc support is alpha.

;;; Major mode hooks

(add-hook 'scrim-mode-hook 'rainbow-delimiters-mode) ;; Optional
(add-hook 'scrim-mode-hook 'clojure-mode-variables)
(add-hook 'scrim-mode-hook 'clojure-font-lock-setup)

;;; Minor mode hooks

;; If you already use clojure-mode.
(add-hook 'clojure-mode-hook 'scrim-minor-mode)
;; If you don't use clojure-mode.
;;(add-to-list 'auto-mode-alist '("\\.clj\\|\\.cljc\\|\\.cljs\\|\\.edn$" . scrim-minor-mode))

;;; Extras

;; Add the hideshow minor mode to the scrim REPL buffer.
;; This has to run after clojure-mode-variables, hence the 'append argument.
(add-hook 'scrim-mode-hook 'hs-minor-mode 'append)

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

  ;; There may be a better place for this.
  (my-scrim-hide-last-input)

  ;; This function may be called multiple times; each call containing a portion
  ;; of the complete output. For example, after receiving the user's input, this
  ;; function may be called with a blank string, then again with the result, and
  ;; then again with a prompt. The result value may also be split across
  ;; multiple calls. In order to display the complete result, the value is read
  ;; from the REPL buffer, instead of using the string argument to this
  ;; function.
  (my-scrim-echo-output))

(defun init-scrim-mode ()
  "My customizations."
  (add-hook 'comint-output-filter-functions 'my-scrim-output-filter nil t))

(add-hook 'scrim-mode-hook 'init-scrim-mode)
```
## Usage

The keybindings can be displayed by either using `C-h m` in a Scrim-enabled buffer and then
selecting `Scrim` under the enabled minor modes, or `C-h f scrim-minor-mode RET`.

## Guides

This repository includes a couple simple sample projects with instructions that walk through the
steps required to get to a working interactive development environment.

* [Clojure](clj-demo)
* [ClojureScript](cljs-demo)

## Bugs, feedback, etc.

Please create an issue here: https://github.com/austinhaas/scrim/issues

## Credit

Similar projects I've used and referenced:

* [SLIME](https://common-lisp.net/project/slime/)
* [Geiser](http://www.nongnu.org/geiser/)
* [inf-clojure](https://github.com/clojure-emacs/inf-clojure)
* [CIDER](https://github.com/clojure-emacs/cider)

## License

Copyright (c) 2020 Austin Haas.

Licensed under the [GNU General Public License, version 3](COPYING).
