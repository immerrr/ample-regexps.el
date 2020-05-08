EMACS ?= emacs
EMACS_VERSION=$(shell $(EMACS) -batch -eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
EMACS_BATCH=cask exec $(EMACS) --batch -Q
AMPLE_REGEXPS_ELC=ample-regexps.$(EMACS_VERSION).elc

.PHONY: test-compiled test-uncompiled compile

dist:
	cask package

compile:
	$(EMACS_BATCH) -f batch-byte-compile ample-regexps.el && mv ample-regexps.elc $(AMPLE_REGEXPS_ELC)

$(AMPLE_REGEXPS_ELC): ample-regexps.el
	make compile

test-uncompiled:
	cask exec ert-runner -l ample-regexps.el

test-compiled: $(AMPLE_REGEXPS_ELC)
	cask exec ert-runner -l $(AMPLE_REGEXPS_ELC)

test: test-uncompiled test-compiled

tryout:
	cask exec $(EMACS) -Q -L . -l init-tryout.el test-arx.el
