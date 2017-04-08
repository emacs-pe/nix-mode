CASK  ?= cask
WGET  ?= wget
EMACS  = emacs

EMACSFLAGS =
EMACSBATCH = $(EMACS) --batch -Q -L . $(EMACSFLAGS)

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS := $(shell EMACS=$(EMACS) $(CASK) files)
OBJS  = $(SRCS:.el=.elc)

.PHONY: all compile clean

all: compile README.md

compile: $(OBJS)

clean:
	$(CASK) clean-elc

test: clean
	$(CASK) exec $(EMACSBATCH) -L . -l nix-mode-test.el -f ert-run-tests-batch-and-exit

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

README.md: el2markdown.el nix-mode.el
	$(CASK) exec $(EMACSBATCH) -l $< nix-mode.el -f el2markdown-write-readme

el2markdown.el:
	$(WGET) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

.INTERMEDIATE: el2markdown.el
