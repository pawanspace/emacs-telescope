EMACS ?= emacs
BATCH := $(EMACS) -Q --batch

SRCS := emacs-telescope.el emacs-telescope-grep.el src/emacs-telescope-ui.el src/emacs-telescope-sources.el src/emacs-telescope-actions.el
TESTS := $(wildcard tests/*.el)

.PHONY: all compile test clean package

all: compile

compile:
	$(BATCH) -L . -f batch-byte-compile $(SRCS)

test:
	$(BATCH) -L . -l ert -l $(TESTS) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc src/*.elc tests/*.elc

package:
	$(BATCH) -L . --eval "(progn (require 'package) (package-initialize) (package-generate-autoloads \"emacs-telescope\" \".\") (package-build-archive \"emacs-telescope\"))"

install: compile
	mkdir -p ~/.emacs.d/site-lisp/emacs-telescope
	cp -r *.el *.elc src ~/.emacs.d/site-lisp/emacs-telescope/
