# This Makefile has largely been taken by looking at the corresponding Makefiles
# from the Magit and the cider projects.

TOP := $(dir $(lastword $(MAKEFILE_LIST)))
LOAD_PATH = -L $(TOP) -L $(TOP)vendor
PKG = writer-mode

ELS_ALL = $(wildcard *.el)
ELS = $(filter-out $(PKG)-autoloads.el,$(ELS_ALL))
OBJECTS = $(ELS:.el=.elc)

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)

# TODO ???? There's also the shut-up package
define suppress_warnings
(fset 'original-message (symbol-function 'message))
(fset 'message ;'
      (lambda (f &rest a)
        (unless (or (equal f "Wrote %s")
                    (equal f "pcase-memoize: equal first branch, yet different")
                    (and (equal f "Warning: Unknown defun property `%S' in %S")
                         (memq (car a) '(pure side-effect-free interactive-only))))
          (apply 'original-message f a))))
endef
export suppress_warnings

##
# General

all: test

.PHONY: version
version:
	@$(EMACS) --version

.PHONY: clean
clean:
	@rm -f elpa-$(EMACS) $(OBJECTS) $(PKG)-autoloads.el? $(TOP)vendor/*

##
# Test

# This task takes care of downloading development/test dependencies into the
# `vendor` directory. Maybe it would've been nicer to use Cask or something like
# that, but it doesn't have too many dependencies so it should be fine for now.
.PHONY: vendor
vendor:
ifeq ("$(wildcard $(TOP)vendor/elisp-lint.el)","")
	@wget -q -O vendor/elisp-lint.el https://raw.githubusercontent.com/gonewest818/elisp-lint/master/elisp-lint.el
endif
ifeq ("$(wildcard $(TOP)vendor/package-lint.el)","")
	@wget -q -O vendor/package-lint.el https://raw.githubusercontent.com/purcell/package-lint/master/package-lint.el
endif
ifeq ("$(wildcard $(TOP)vendor/visual-fill-column.el)","")
	@wget -q -O vendor/visual-fill-column.el https://raw.githubusercontent.com/joostkremers/visual-fill-column/master/visual-fill-column.el
endif
ifeq ("$(wildcard $(TOP)vendor/writeroom-mode.el)","")
	@wget -q -O vendor/writeroom-mode.el https://raw.githubusercontent.com/joostkremers/writeroom-mode/master/writeroom-mode.el
endif
ifeq ("$(wildcard $(TOP)vendor/olivetti.el)","")
	@wget -q -O vendor/olivetti.el https://raw.githubusercontent.com/rnkn/olivetti/master/olivetti.el
endif

.PHONY: test
test: lint git-validation unit-test

.PHONY: git-validation
git-validation:
ifeq (, $(shell which git-validation 2> /dev/null))
	@echo "You don't have 'git-validation' installed, consider installing it (see the CONTRIBUTING.org file)."
else
	@git-validation -q -range b910b01ecc0c..HEAD
endif

.PHONY: unit-test
unit-test: version
	@$(BATCH) --eval "(progn\
	(load-file \"t/writer-tests.el\")\
	(ert-run-tests-batch-and-exit))"

.PHONY: lint
lint: version vendor elisp-lint package-lint

.PHONY: elisp-lint
elisp-lint:
	@$(BATCH) -l elisp-lint.el -f elisp-lint-files-batch $(ELS)

.PHONY: package-lint
package-lint:
	@$(BATCH) -l package-lint.el -f package-lint-batch-and-exit $(ELS)
