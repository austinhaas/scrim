#/bin/bash

# Compile with advanced optimizations.

clj --main cljs.main \
    --optimizations advanced \
    --compile cljs-demo.core

exit 0
