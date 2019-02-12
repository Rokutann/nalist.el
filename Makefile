EMACS ?= emacs
CASK ?= cask

docs:
	${CASK} exec ${EMACS} -Q --script bin/docs.el
