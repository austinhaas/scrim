REPL_SERVER_ADDRESS := "127.0.0.1"
REPL_SERVER_PORT := 5555

clojure := clojure

.PHONY : clj-repl
clj-repl : ## Start a Clojure REPL socket server
	@echo "Starting REPL server on $(REPL_SERVER_ADDRESS):$(REPL_SERVER_PORT)"
	@echo "Ctrl-c to quit."
	@$(clojure) -X:clj-repl-server :address '$(REPL_SERVER_ADDRESS)' :port $(REPL_SERVER_PORT)

.PHONY: clean
clean : ## Remove temporary files
	rm -rf .cpcache

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-]+ ?:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-9s\033[0m %s\n", $$1, $$2}'
