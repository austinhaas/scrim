# A guide to developing with ClojureScript, Emacs, and Scrim

This will show you how to connect all of the pieces and test that everything works.

### Features

#### Source code compilation

  ClojureScript code is compiled once, then recompiled when files change. Recompiling keeps the compiled code fresh (for instance, when the browser is refreshed).

#### Run-time code evaluation in Emacs buffers

  With a keystroke, Emacs sends ClojureScript code to a Java process running a ClojureScript REPL, which compiles the code into Javascript, then sends it to the browser to be executed.

### Prerequisites

* [Clojure and CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* [Emacs](https://www.gnu.org/software/emacs/)
* [Scrim](https://github.com/austinhaas/scrim)
* [A web browser](https://www.mozilla.org/en-US/firefox/new/)

### Step 1: Open a terminal and navigate to where scrim was installed.
```
cd <scrim-install-dir>/cljs-demo/
```

### Step 2: Start a clojure process
```
make cljs-node-repl
```
or
```
make clj-browser-repl
```

### Step 3: Open the ClojureScript file in Emacs
```
C-x C-f scrim-install-dir/cljs-demo/src/cljs_demo/demo.cljs RET
```

### Step 4: Connect Scrim
```
M-x scrim-connect RET localhost RET 5555
```

### Step 5: Evaluate code in Emacs



## Debugging

### Test without Emacs and Scrim

```
telnet localhost 5555
```

### View all communication between Scrim and the Java process
```
sudo ngrep -d any port 5555
```
