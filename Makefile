all: build

build:
	ocamlbuild -use-ocamlfind \
	-I src -pkgs ezjsonm,bisect,unix \
	ocveralls.native

install: build
ifndef bindir
	$(error bindir is not set)
else
	cp ocveralls.native $(bindir)/ocveralls
endif

clean:
	ocamlbuild -clean
