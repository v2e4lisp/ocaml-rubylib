all: pp

pp: pp.ml
	ocamlfind ocamlopt -o pp -linkpkg -package lang_ruby pp.ml

clean:
	rm -f pp *.cm[ixo] *.o
