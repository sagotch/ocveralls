all: ocveralls

ocveralls:
	ocamlbuild -I src -pkg bisect ocveralls.native

clean:
	ocamlbuild -clean
