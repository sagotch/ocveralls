all: build

# Choose where we get the runtime based on what's installed
ifeq ($(shell ocamlfind query bisect >/dev/null 2>&1 && echo y),y)
BISECT=bisect
else ifeq ($(shell ocamlfind query bisect_ppx >/dev/null 2>&1 && echo y),y)
BISECT=bisect_ppx
endif

build:
ifndef BISECT
	$(error No bisect runtime)
else
	ocamlbuild -use-ocamlfind \
		-I src -pkgs ezjsonm,$(BISECT),unix,str \
		ocveralls.native
endif

install: build
ifndef bindir
	$(error bindir is not set)
else
	cp ocveralls.native $(bindir)/ocveralls
endif

clean:
	ocamlbuild -clean
