all: coveralls

coveralls:
	ocamlbuild -I src -pkg bisect coveralls.native

clean:
	ocamlbuild -clean
