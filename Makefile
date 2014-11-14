############
OCAMLC=/usr/bin/env ocamlc
OCAMLC_FLAGS=
OCAMLLEX=/usr/bin/env ocamllex
OCAMLYACC=/usr/bin/env ocamlyacc
############
all: micro

parser.ml: parser.mly
	$(OCAMLYACC) $<

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

micro: micro.ml codegen.ml lexer.ml parser.ml
	$(OCAMLC) $(OCAMLC_FLAGS) parser.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ str.cma codegen.ml parser.ml lexer.ml  micro.ml

############
clean:
	rm -f codegen.cmo
	rm -f codegen.cmi
	rm -f lexer.ml
	rm -f lexer.cmo
	rm -f lexer.cmi
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.cmo
	rm -f parser.cmi
	rm -f micro
	rm -f micro.cmi
	rm -f micro.cmo
