PRODUCTS=\
blueprint.cmi blueprint.cmxa blueprint.cmxs \
blueprint.cmi blueprint.cmti \
xmlRope.cmi xmlRope.cmti
.PHONY: all lib tool install clean blue.native $(PRODUCTS) test

OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y,-bin-annot \
		-Is cli,unix,lib

DESCRN=$(shell [ -d .git ] && git describe --tags --always || cat blueprint.version)
DESCR=$(shell echo "$(DESCRN)" | tr -d '\n')

DIRTY_FLAG=$(shell [ -d .git ] && git diff-index --quiet HEAD || echo "dirty")
ifeq ($(DIRTY_FLAG),dirty)
DIRTY=true
else
DIRTY=false
endif

all: lib tool
lib: $(PRODUCTS)
tool: blue

install: lib lib/META
	ocamlfind install blueprint lib/META lib/blueprint.mli \
		$(addprefix _build/lib/,$(PRODUCTS))

lib/META: lib/META.in
	sed -e "s/%{blueprint:version}%/$(DESCR)/" < lib/META.in > lib/META

blue.native: blueprintVersion.ml
	$(OCAMLBUILD) blue.native

blueprint.cma:
	$(OCAMLBUILD) blueprint.cma

blueprint.cmxa:
	$(OCAMLBUILD) blueprint.cmxa

blueprint.cmxs:
	$(OCAMLBUILD) blueprint.cmxs

blue: blue.native
	ln -f blue.native blue

blueprintVersion.ml:
	printf "let git_descr = \""              > blueprintVersion.ml
	printf "$(DESCR)"                       >> blueprintVersion.ml
	printf "\"\nlet git_dirty = $(DIRTY)\n" >> blueprintVersion.ml

test:
	$(MAKE)
	$(MAKE) -C test
	$(MAKE) -C test test

clean:
	ocamlbuild -clean
	rm -f blueprintVersion.ml blue lib/META
	$(MAKE) -C test clean
