.PHONY: all clean blue.native test

DIRTY_FLAG=$(shell git diff-index --quiet HEAD || echo "dirty")
ifeq ($(DIRTY_FLAG),dirty)
DIRTY=true
else
DIRTY=false
endif

all: blue

blue.native: blueprintVersion.ml
	ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y -Is cli,unix,lib blue.native

blue: blue.native
	ln -f blue.native blue

blueprintVersion.ml:
	printf "let git_rev = \""                > blueprintVersion.ml
	git rev-parse HEAD | tr -d '\n'         >> blueprintVersion.ml
	printf "\"\nlet git_dirty = $(DIRTY)\n" >> blueprintVersion.ml

test:
	$(MAKE)
	$(MAKE) -C test
	$(MAKE) -C test test

clean:
	ocamlbuild -clean
	rm -f blueprintVersion.ml blue
	$(MAKE) -C test clean
