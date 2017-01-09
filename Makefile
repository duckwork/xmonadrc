# based on (github.com/pjones/xmonadrc)

export TMPDIR = /tmp
TMP_DUMMY = $(TMPDIR)/dummy

STACK_OPTS = #--extra-lib-dirs=/usr/lib --extra-include-dirs=/usr/include/X11/Xft
DIST_PATH = $(shell stack $(STACK_OPTS) path --dist-dir)
PKG_ROOT_PATH = $(shell stack $(STACK_OPTS) path --local-install-root)

ARCH = $(shell uname -m)
OS = $(shell uname -s | tr '[A-Z]' '[a-z]')

XMONAD = $(PKG_ROOT_PATH)/bin/xmonad
XMONAD_SRC = $(DIST_PATH)/build/xmonadrc/xmonadrc
XMONAD_DEST = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)

.PHONY: all install restart clean

all: $(TMPDIR)
	stack $(STACK_OPTS) setup
	stack $(STACK_OPTS) build
	# hlint src *.hs

clean:
	rm -rf .stack-work build/.stack-work
	rm -rf dist dist-newstyle

install: all
	mkdir -p $(dir $(XMONADRC_DEST))
	install -m 0755 $(XMONADRC_SRC) $(XMONADRC_DEST)
	install -m 0755 $(XMONAD) $(dir $(XMONADRC_DEST))/xmonad

restart: install
	$(XMONADRC_DEST) --restart

$(TMP_DUMMY):
	mkdir -p $(dir $@)
	touch $@
