.EXPORT_ALL_VARIABLES:
.PHONY: test

SHELL = bash

init:
	 echo 'TBD'

repl:
	clj -M:test:nrepl -m nrepl.cmdline --middleware [cider.nrepl/cider-middleware]

test:
	clojure -M:test:kaocha

proxy:
	kubectl proxy --port=8080 --disable-filter

pom:
	clj -X:deps mvn-pom

jar:
	clojure -X:depstar jar :jar target/zen-ops.jar

uberjar:
	clojure -X:depstar uberjar :jar target/zen-doc.uber.jar

pub:
	clj -A:deploy
