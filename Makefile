# This Makefile has largely been taken by looking at the corresponding Makefiles
# from the Magit and the cider projects.

TOP := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
LOAD_PATH = -L $(TOP) -L $(TOP)vendor -L $(TOP)vendor/org-mode/lisp
TEST_LOAD_PATH = -L $(TOP)t
PKG = writer-mode

ELS_ALL = $(wildcard *.el)
ELS = $(filter-out $(PKG)-autoloads.el,$(ELS_ALL))
TESTS_ELS_ALL = $(wildcard t/*.el)
TESTS_ELS = $(filter-out t/t-autoloads.el,$(TESTS_ELS_ALL))
TESTS_ELS_NO_DIR = $(notdir $(TESTS_ELS))
OBJECTS = $(ELS:.el=.elc)

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)

##
# General

all: clean test

.PHONY: version
version:
	@$(EMACS) --version

.PHONY: clean
clean:
	@rm -rf elpa-$(EMACS) $(OBJECTS) $(PKG)-autoloads.el* $(TOP)vendor/* $(TOP)t/*.elc $(TOP)t/t-autoloads.el* $(TOP)t/tmp/*

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
ifeq ("$(wildcard $(TOP)vendor/dash.el)","")
	@wget -q -O vendor/dash.el https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
endif
ifeq ("$(wildcard $(TOP)vendor/org-mode)","")
	@cd vendor && git clone -b 'release_9.1.14' --depth 1 https://code.orgmode.org/bzg/org-mode && cd org-mode && make autoloads
endif

.PHONY: test
test: vendor lint git-validation unit-test

.PHONY: git-validation
git-validation:
ifeq (, $(shell which git-validation 2> /dev/null))
	@echo "You don't have 'git-validation' installed, consider installing it (see the CONTRIBUTING.org file)."
else
	@git-validation -q -range b910b01ecc0c..HEAD -travis-pr-only=false
endif

.PHONY: unit-test
unit-test: version unit-test-non-interactive unit-test-interactive

.PHONY: unit-test-non-interactive
unit-test-non-interactive:
	@$(BATCH) $(TEST_LOAD_PATH) -l ert -l t/writer-tests.el -f ert-run-tests-batch-and-exit

.PHONY: unit-test-interactive
unit-test-interactive:
	@rm -f $(TOP)t/tmp/interactive-results.txt $(TOP)t/*.elc $(TOP)t/t-autoloads.el
	@$(EMACS) -Q $(LOAD_PATH) $(TEST_LOAD_PATH) -nw -l t/writer-interactive-tests -f writer-interactive-tests-run
	@cat $(TOP)t/tmp/interactive-results.txt

.PHONY: lint
lint: version elisp-lint package-lint

.PHONY: elisp-lint
elisp-lint:
	@$(BATCH) -l elisp-lint.el -f elisp-lint-files-batch $(ELS)
# For some reason `elisp-lint` fails if we put $(TEST_ELS) inside of
# $(ELS). Thus, we have to actually move into the `t` directory and execute the
# same thing again but with $(TESTS_ELS_NO_DIR).
	@cd t && $(BATCH) -l elisp-lint.el -f elisp-lint-files-batch $(TESTS_ELS_NO_DIR)

.PHONY: package-lint
package-lint:
# HACK: this ugly `sed` command is to workaround a problem of package-lint and
# GNU Emacs 26.x where package-lint doesn't seem to know about org-mode, even
# though all the test suite is fine and (package-install 'org) warns us that
# it's already installed.
	@sed -i 's| (org "9.1")||g' writer.el writer-notes.el writer-org.el
	@$(BATCH) -l package-lint.el -f package-lint-batch-and-exit $(ELS) $(TESTS_ELS)
