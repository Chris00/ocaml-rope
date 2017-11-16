PKGVERSION = $(shell git describe --always --dirty)

WEB = rope.forge.ocamlcore.org:/home/groups/rope/htdocs
SRC_WEB	= web

all build:
	jbuilder build @install #--dev
	jbuilder build bench/bm_ropes.exe bench/bench_rope.exe

test runtest:
# Force the tests to be run
	$(RM) -rf _build/default/tests/
	jbuilder runtest

install uninstall:
	jbuilder $@

# Benchmarks
bench:
	jbuilder build @bench \
	  && cd _build/default/bench/ && gnuplot -persist bm_ropes.plot
	@echo "Bench results: SGV in _build/default/bench/"

# Release a Sourceforge tarball and publish the HTML doc
.PHONY: web upload
web-doc: doc
	@ if [ -d API.docdir/ ] ; then \
	  scp -r API.docdir/ $(WEB)/ \
	  && echo "*** Published documentation" ; \
	fi
web:
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  scp $(addprefix $(SRC_WEB)/, *.html *.css *.png) LICENSE $(WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/)" ; \
	fi

clean::
	jbuilder clean
	$(RM) $(wildcard bench/*.dat)

.PHONY: all build test runtest bench clean
