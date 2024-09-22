EMACS := emacs

.PHONY: all
all: byte-compile test

.PHONY: clean
clean:
	@-rm dbml-mode*.elc 2>/dev/null
	@-rm *.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
		&& test -f "$@"

byte-compile: \
	dbml-mode.elc \
	dbml-mode-tests.elc

.PHONY: test
test: byte-compile main-tests

dbml-mode-tests.ok: \
	dbml-mode.elc dbml-mode-tests.elc test-files
	$(EMACS) --batch --quick \
		--directory . \
		--load dbml-mode-tests.el \
		--funcall ert-run-tests-batch \
	&& touch dbml-mode-tests.ok
main-tests: dbml-mode-tests.ok

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" dbml-mode.el \
		| tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
