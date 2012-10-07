all: pp

pp: pp.ml
	ocamlfind ocamlopt -o pp -linkpkg -package rubylib pp.ml

clean:
	rm -f pp *.cm[ixo] *.o
