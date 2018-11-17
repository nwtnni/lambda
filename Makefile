.PHONY: default clean

default:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	rm -rf main.byte _build
