all: ocveralls

ocveralls:
	ocamlbuild -I src -pkg ezjsonm -pkg bisect ocveralls.native

clean:
	ocamlbuild -clean
