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
cd ~/.emacs.d/site-lisp  # or wherever you keep locally installed Emacs packages
git clone git@github.com:austinhaas/scrim.git
```
### Update your .emacs

I've included a couple init files that you can use as reference. This is what I
use to initialize scrim. You will probably only want to take what you want and
modify as necessary.

* [A minimal configuration](scrim-init-minimal.el)
* [Extra](scrim-init-extra.el) (experimental)

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

Copyright (c) 2021 Austin Haas.

Licensed under the [GNU General Public License, version 3](COPYING).
