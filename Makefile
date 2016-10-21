CC=closure-compiler -O ADVANCED_OPTIMIZATIONS
ROOT=$(shell stack path --local-install-root)

all: dbtool ertool

build:
	stack build

dbtool: build
	${CC} ${ROOT}/bin/db-tool-dom.jsexe/all.js > ~/wiki/js/dbtool.min.js

ertool: build
	${CC} ${ROOT}/bin/er-tool-dom.jsexe/all.js > ~/wiki/js/ertool.min.js
