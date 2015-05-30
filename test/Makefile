.PHONY: all test clean

all: test.native

test.native: test.ml
	ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y test.native

test: test.native
	./test.native

clean:
	ocamlbuild -clean
