# A guide to developing with Emacs, scrim, and ClojureScript

This is intended to be minimal.

## Prerequisites

* [Clojure and CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* [Emacs](https://www.gnu.org/software/emacs/)
* [scrim](https://github.com/austinhaas/scrim)
* [Python](https://www.python.org/)(for a simple webserver)
* [A web browser](https://www.mozilla.org/en-US/firefox/new/)

## Step 1: Open a terminal and navigate to the cljs-demo root

(where this file is located)

```
cd scrim-install-dir/cljs-demo/
```

## Step 2: Launch a clojure dev process

This process will:
* Compile the cljs code
* Watch for changes and recompile
* Start a socket server.

```
./run-dev.sh
```

See the contents of that file for more info.

## Step 3: Launch a webserver
```
python3 -m http.server
```

`clj` can launch a simple webserver, but we don't use it because it has a few issues:
- [Doesn't serve some files](https://clojure.atlassian.net/browse/CLJS-2433) (.glb, in my case).
- Specifying the port doesn't work; port is always 9000.
- Can't specify which directory to serve.
- Prevents launching a cljs repl for some reason.

## Step 4: Open src/cljs_demo/core.cljs in Emacs

## Step 5: Connect scrim
```
M-x scrim-connect RET localhost RET 5555
```

## Step 6: Open the app in a web browser
```
firefox http://0.0.0.0:8000
```

## Step 7: Use Emacs/scrim to evaluate code

Switch to the cljs-demo.core namespace:
```
C-c C-n
```

Move the cursor just past the closing paren of any expression and evaluate it:
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

### Test everything but Emacs and scrim

Do the steps above, but instead of #4 and #5, do:

```
telnet localhost 5555
```
