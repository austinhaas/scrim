# SCRIM - Simple Clojure REPL Interaction Mode

An Emacs package for Clojure REPL interaction.

A major mode for connection to a Clojure REPL process in a buffer.

A minor mode for buffers containing Clojure(script) code.

No magic. No extras.

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

## Installation

### Clone the repository

```sh
cd ~/.emacs.d/site-lisp  # or wherever you keep locally installed emacs packages
git clone git@github.com:austinhaas/scrim.git
```
### Update your .emacs

```
(add-to-list 'load-path "~/.emacs.d/site-lisp/scrim/") ;; or wherever you put it
(require 'scrim)
;; If you already use clojure-mode.
(add-hook 'clojure-mode-hook #'scrim-minor-mode)
;; If you don't use clojure-mode.
;;(add-to-list 'auto-mode-alist '("\\.clj\\|\\.cljc\\|\\.cljs\\|\\.edn$" . scrim-minor-mode))
;; Some optional improvements to the REPL buffer.
(add-hook 'scrim-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scrim-mode-hook #'clojure-mode-variables)
(add-hook 'scrim-mode-hook #'clojure-font-lock-setup)
```
## Usage

Use `C-h m` in a buffer to see keybindings.

## Bugs, feedback, etc.

Please open a ticket in github.

## Credit

Similar projects I've used and referenced:

* [SLIME](https://common-lisp.net/project/slime/)
* [Geiser](http://www.nongnu.org/geiser/)
* [inf-clojure](https://github.com/clojure-emacs/inf-clojure)
* [CIDER](https://github.com/clojure-emacs/cider)

## License

Copyright (c) 2020 Austin Haas.

Licensed under the [GNU General Public License, version 2](blob/master/LICENSE.txt).
