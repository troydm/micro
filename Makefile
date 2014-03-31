############
OCAMLC=/usr/bin/env ocamlc
OCAMLC_FLAGS=
############
all: micro

micro: micro.ml 
	$(OCAMLC) $(OCAMLC_FLAGS) -o micro unix.cma $<

############
clean:
	rm -f micro.cmi
	rm -f micro.cmo
	rm -f micro
