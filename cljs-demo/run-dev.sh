#/bin/bash

# Compile once, then watch 'src' and compile after any changes.

# Open a socket server on $SOCKET_PORT, and launch a cljs browser REPL on connect.

# https://clojurescript.org/reference/repl-options#warn-on-undeclared
OPTS="{:warn-on-undeclared false}"

REPL_OPTS="{:launch-browser false}"

SOCKET_PORT=5555

SOCKET_OPTS="{:port $SOCKET_PORT :accept cljs.server.browser/repl :args [{:opts $OPTS :env-opts $REPL_OPTS}]}"

clj -J-Dclojure.server.myrepl="$SOCKET_OPTS" \
    --main cljs.main \
    --optimizations none \
    --watch src \
    --compile cljs-demo.core

exit 0
