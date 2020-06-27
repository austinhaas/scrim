# A guide to developing with Clojure, Emacs, and Scrim

How to connect all of the pieces and test that everything works.

This is what I consider to be the base of an ideal Clojure development environment. It is minimal, but powerful.

### Prerequisites

* [Clojure and CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* [Emacs](https://www.gnu.org/software/emacs/)
* [Scrim](https://github.com/austinhaas/scrim)

### Step 1: Open a terminal and navigate to the project root

(where this file is located)

```
cd scrim-install-dir/clj-demo/
```

### Step 2: Start a clojure process

This process will start a socket server.
```
./run-dev.sh
```

This shell script just wraps `clj` with a few options. [See the contents of that file for more info.](run-dev.sh)

### Step 3: Open the Clojure file in Emacs and follow instructions there.
```
C-x C-f scrim-install-dir/clj-demo/src/clj_demo/core.clj RET
```

## More useful commands

### Test without Emacs and Scrim

```
telnet localhost 5555
```

### View all communication between Scrim and the Java process
```
sudo ngrep -d any port 5555
```
