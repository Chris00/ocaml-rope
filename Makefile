PKGVERSION = $(shell git describe --always --dirty)

all build:
	dune build @install bench/bm_ropes.exe bench/bench_rope.exe

test runtest:
	dune runtest --force

install uninstall:
	dune $@

doc:
	dune build @doc

# Benchmarks
bench:
	dune build @bench \
	  && cd _build/default/bench/ && gnuplot -persist bm_ropes.plot
	@echo "Bench results: SGV in _build/default/bench/"

web:
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  scp $(addprefix $(SRC_WEB)/, *.html *.css *.png) LICENSE $(WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/)" ; \
	fi

clean::
	dune clean
	$(RM) $(wildcard bench/*.dat)

.PHONY: all build test runtest doc bench clean
