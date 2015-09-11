.PHONY: recompile build install uninstall

ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

recompile:
	xmonad --recompile

build:
	cabal build

install:
	mkdir -p ~/.xmonad
	ln -s $(ROOT)/src/xmonad.hs ~/.xmonad/xmonad.hs
	ln -s $(ROOT)/src ~/.xmonad/lib
	ln -s $(ROOT)/src/xmobar.hs ~/.xmobarrc

uninstall:
	rm -f ~/.xmonad/xmonad.hs
	rm -f ~/.xmonad/lib
	rm -f ~/.xmobarrc

