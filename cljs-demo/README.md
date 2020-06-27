# A guide to developing with ClojureScript, Emacs, and scrim

How to connect all of the pieces and test that everything works.

This is what I consider to be the base of an ideal ClojureScript development environment. It is minimal, but powerful.

### Features

#### Source code compilation

  ClojureScript code is compiled once, then recompiled when files change. Recompiling keeps the compiled code fresh (for instance, when the browser is refreshed).

#### Run-time code evaluation in Emacs buffers

  With a keystroke, Emacs sends ClojureScript code to a Java process running a ClojureScript REPL, which compiles the code into Javascript, then sends it to the browser to be executed.

### Prerequisites

* [Clojure and CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* [Emacs](https://www.gnu.org/software/emacs/)
* [scrim](https://github.com/austinhaas/scrim)
* [Python](https://www.python.org/) (for a simple webserver)
* [A web browser](https://www.mozilla.org/en-US/firefox/new/)

### Step 1: Open a terminal and navigate to the project root

(where this file is located)

```
cd scrim-install-dir/cljs-demo/
```

### Step 2: Start a clojure process

This process will:
* Compile the cljs code
* Watch for changes and recompile
* Start a socket server

```
./run-dev.sh
```

This shell script just wraps `clj` with a bunch of options. [See the contents of that file for more info.](run-dev.sh)

### Step 3: Launch a webserver
```
python3 -m http.server --bind localhost 8000
```

> `clj` can launch a simple webserver, but we don't use it because:
> - [Some file types aren't supported](https://clojure.atlassian.net/browse/CLJS-2433) (.glb, in my case)
> - Specifying the port doesn't work; port is always 9000
> - Can't specify which directory to serve
> - Conflicts with launching a cljs REPL

### Step 4: Open the ClojureScript file in Emacs
```
C-x C-f scrim-install-dir/cljs-demo/src/cljs_demo/core.cljs RET
```

### Step 5: Connect scrim
```
M-x scrim-connect RET localhost RET 5555
```

### Step 6: Open the app in a web browser
```
firefox http://localhost:8000
```

### Step 7: Evaluate code in Emacs

Switch to the cljs-demo.core namespace
```
C-c C-n
```

Move the cursor just past the closing paren of any expression and evaluate it
```
C-C C-e
```

## More useful commands

### clean
```
rm -rf out
```

### Create a production build
```
./compile-advanced.sh
```

This shell script just wraps `clj` with a few options. [See the contents of that file for more info.](compile-advanced.sh)

### Test everything but Emacs and scrim

Do the steps above, but instead of Step 4 and Step 5, do

```
telnet localhost 5555
```
and enter clojurescript expressions at the prompt.
