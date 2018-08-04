.PHONY: default

default:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte
