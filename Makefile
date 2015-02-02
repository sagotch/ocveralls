all: build

build:
	ocamlbuild -I src -pkgs ezjsonm,bisect ocveralls.native

install: build
ifndef bindir
	$(error bindir is not set)
else
	cp ocveralls.native $(bindir)/ocveralls
endif

clean:
	ocamlbuild -clean
