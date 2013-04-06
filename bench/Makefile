.PHONY: default byte bench bench.byte
default: bench
byte: bench.byte

bench: bm_ropes.native
	./$< && gnuplot -persist bm_ropes.plot

bench.byte: bm_ropes.byte
	./$< && gnuplot -persist bm_ropes.plot
