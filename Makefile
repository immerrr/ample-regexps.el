EMACS ?= emacs
EMACS_BATCH=cask exec $(EMACS) --batch -Q

.PHONY: test-compiled test-uncompiled

dist:
	cask package

%.elc: %.el
	$(EMACS_BATCH) -f batch-byte-compile ample-regexps.el

test-uncompiled:
	cask exec ert-runner -l ample-regexps.el

test-compiled: ample-regexps.elc
	cask exec ert-runner -l ample-regexps.elc

test: test-compiled test-uncompiled

tryout:
	cask exec $(EMACS) -Q -L . -l init-tryout.el test-arx.el
