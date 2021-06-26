REPL_SERVER_ADDRESS := "127.0.0.1"
REPL_SERVER_PORT := 5555

clojure := clojure

.PHONY : cljs-node-repl
cljs-node-repl : ## Start a ClojureScript REPL socket server (Node)
	@echo "Starting cljs node REPL server on $(REPL_SERVER_ADDRESS):$(REPL_SERVER_PORT)"
	@echo "Ctrl-c to quit."
	@$(clojure) -X:cljs-node-repl-server :address '$(REPL_SERVER_ADDRESS)' :port $(REPL_SERVER_PORT)

.PHONY : cljs-browser-repl
cljs-browser-repl : ## Start a ClojureScript REPL socket server (browser)
	@echo "Starting cljs browser REPL server on $(REPL_SERVER_ADDRESS):$(REPL_SERVER_PORT)"
	@echo "Ctrl-c to quit."
	@$(clojure) -X:cljs-browser-repl-server :address '$(REPL_SERVER_ADDRESS)' :port $(REPL_SERVER_PORT)

.PHONY: clean
clean : ## Remove temporary files
	rm -rf .cljs_node_repl
	rm -rf .cpcache
	rm -rf out

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-]+ ?:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
