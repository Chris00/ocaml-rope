VERSION=0.2

UNSAFE=-unsafe -noassert # speed gained by this is small
OCAMLC_FLAGS= -dtypes -g $(UNSAFE)
OCAMLOPT_FLAGS= -dtypes -inline 3 $(UNSAFE)

DIST_FILES=rope.ml rope.mli rope_top.ml LICENSE Makefile

TARBALL_DIR=data-struct-$(VERSION)
TARBALL=$(TARBALL_DIR).tar.bz2

BENCHMARK_INC= -I $(HOME)/software/OCaml/benchmark/

default:
	@echo -n "This project now uses 'ocamlbuild' for the build phase.  "
	@echo -e "Please type\n\tocamlbuild all.otarget"
	@echo "(If you nonetheless want to use 'make' type 'make all'.)"

all: rope.cma rope.cmxa

bench_rope.native: rope.cmxa bench_rope.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS=) $(BENCHMARK_INC) \
	  unix.cmxa benchmark.cmxa  $^

doc: $(wildcard *.mli)
	[ -d "$@" ] || mkdir $@
	$(OCAMLDOC) -html -d $@ $^

dist: $(TARBALL)
$(TARBALL):
	mkdir $(TARBALL_DIR)
	cp $(DIST_FILES) $(TARBALL_DIR)/
	tar -jvcf $@ $(TARBALL_DIR)
	rm -rf $(TARBALL_DIR)


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
	rm -rf doc/
	find . -type f -perm -u=x -exec rm -f {} \;
