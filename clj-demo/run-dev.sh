#/bin/bash

# Open a socket server on $SOCKET_PORT, and launch a Clojure REPL on connect.

SOCKET_PORT=5555

SOCKET_OPTS="{:port $SOCKET_PORT :accept clojure.core.server/repl}"

clj -J-Dclojure.server.myrepl="$SOCKET_OPTS" \
    --repl

exit 0
