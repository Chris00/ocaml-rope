UNSAFE=-noassert -unsafe
LIBS=unix.cmxa
INC=-I ..

OCAMLOPT_FLAGS=-dtypes -inline 10 $(UNSAFE) $(INC)
OCAMLC_FLAGS=-g $(INC)

.PHONY: default byte bench bench.byte
default: bench
byte: bench.byte

bm_ropes.native: ../rope.cmxa tinyRope.cmx bm_ropes.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS) $^

bm_ropes.byte: ../rope.cma tinyRope.cmo bm_ropes.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS:.cmxa=.cma) $^

bench: bm_ropes.native
	./$< && gnuplot -persist bm_ropes.plot

bench.byte: bm_ropes.byte
	./$< && gnuplot -persist bm_ropes.plot


OCAMLC     ?= ocamlc
OCAMLOPT   ?= ocamlopt
OCAMLDEP   ?= ocamldep
OCAMLDOC   ?= ocamldoc
OCAMLFIND  ?= ocamlfind
OCAMLTAGS  ?= ocamltags

# Caml general dependencies
.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(PP) $(OCAMLC_FLAGS) -c $<

%.cma: %.cmo
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) $(PP) $(OCAMLOPT_FLAGS) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPT_FLAGS) $<

%.byte: %.ml
	$(OCAMLC) -o $@ $(PP) $(OCAMLC_FLAGS) $<

%.native: %.ml
	$(OCAMLOPT) -o $@ $(PP) $(OCAMLOPT_FLAGS) $<


.depend.ocaml: $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend.ocaml


.PHONY: clean
clean::
	$(RM) -f *.cm{i,o,x,a,xa} *.annot *.o *.a *~
	$(RM) *.dat *.png
	find . -type f -perm -u=x -exec rm -f {} \;
