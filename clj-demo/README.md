# A demo project for developing with Clojure, Emacs, and Scrim

This will show you how to connect all of the pieces and test that everything works.

### Prerequisites

* [Clojure and CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* [Emacs](https://www.gnu.org/software/emacs/)
* [Scrim](https://github.com/austinhaas/scrim)

### Step 1: Open a terminal and navigate to where scrim was installed.
```
cd <scrim-install-dir>/clj-demo/
```

### Step 2: (Optional) Start a Clojure REPL socket server.
```
make clj-repl
```

To see what this command does, run `make -n clj-repl` or [read the makefile](makefile).

### Step 3: Open the Clojure file in Emacs and follow instructions there.
```
C-x C-f <scrim-install-dir>/clj-demo/src/clj_demo/demo.clj RET
```

## Debugging (when using the socket server)

### Test without Emacs and Scrim

```
telnet localhost 5555
```

### View all communication between Scrim and the Java process
```
sudo ngrep -d any port 5555
```
