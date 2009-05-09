VERSION = $(shell grep "@version" rope.mli | \
		sed -r -e "s/[^0-9.]*([0-9.]+).*/\1/")
SF_WEB  = /home/groups/o/oc/ocaml-rope/htdocs
SRC_WEB	= web

#UNSAFE=-unsafe -noassert # speed gained by this is small
OCAMLC_FLAGS= -dtypes -g $(UNSAFE)
OCAMLOPT_FLAGS= -dtypes -inline 3 $(UNSAFE)

DIST_FILES= rope.ml rope.mli rope_top.ml LICENSE Makefile

TARBALL = rope-$(VERSION).tar.bz2

BENCHMARK_INC= -I $(HOME)/software/OCaml/benchmark/

default:
	@echo -n "This project now uses 'ocamlbuild' for the build phase.  "
	@echo -e "Please type\n\tocamlbuild all.otarget"
	@echo "(If you nonetheless want to use 'make' type 'make all'.)"

all: rope.cma rope.cmxa

bench_rope.native: rope.cmxa bench_rope.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS=) $(BENCHMARK_INC) \
	  unix.cmxa benchmark.cmxa  $^

.PHONY: bench bench.byte
bench: rope.cmxa
	cd bench; $(MAKE)
bench.byte: rope.cma
	cd bench; $(MAKE) byte

META: META.in
	cp $^ $@
	echo "version=\"$(VERSION)\"" >> $@

doc: $(wildcard *.mli)
	[ -d "$@" ] || mkdir $@
	$(OCAMLDOC) -html -d $@ $^

# "Force" a tag to be defined for each released tarball
tar: $(TARBALL)
$(TARBALL):
	bzr export $(TARBALL) -r "tag:$(VERSION)"
	@echo "Created tarball '$(TARBALL)'."


# Release a Sourceforge tarball and publish the HTML doc
.PHONY: web upload
web-doc: doc
	@ if [ -d doc ] ; then \
	  scp -r doc/ shell.sf.net:$(SF_WEB)/ \
	  && echo "*** Published documentation on SF" ; \
	fi
web:
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  scp $(SRC_WEB)/*.html $(SRC_WEB)/*.css $(SRC_WEB)/*.png LICENSE \
	    shell.sf.net:$(SF_WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/) on SF" ; \
	fi


OCAMLC     ?= ocamlc
OCAMLOPT   ?= ocamlopt
OCAMLDEP   ?= ocamldep
OCAMLDOC   ?= ocamldoc
OCAMLFIND  ?= ocamlfind
OCAMLTAGS  ?= ocamltags

# Caml general dependencies
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
	$(RM) -f *.cm{i,o,x,a,xa} *.annot *.o *.a *~ META _log $(TARBALL)
	rm -rf doc/ _build/
	find bench -type f -perm -u=x -exec rm -f {} \;
	cd bench; $(MAKE) clean