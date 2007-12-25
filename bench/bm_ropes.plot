hardcopy = 1

if (hardcopy) set terminal png size 650,800
set grid

set logscale x
if (hardcopy) set output "append.png"; else \
  set terminal wxt 1 enhanced raise
set multiplot layout 2,1 downwards
set key right
set title "Min. unitary append time"
plot [:] [0.5e-7:5.7e-7] \
   "append.dat" using 1:2 with linespoints title "Tiny (unbalanced)", \
   "append.dat" using 1:4 with linespoints title "FullFeatured (unbalanced)", \
   "append-balanced.dat" using 1:2 with lines title "Tiny (balanced)", \
   "append-balanced.dat" using 1:4 with lines title "FullFeatured (balanced)"
set key bottom right
set title ""
set ylabel "depth (after appends)"
plot \
   "append.dat" using 1:3 with linespoints title "Tiny (unbalanced)", \
   "append.dat" using 1:5 with linespoints title "FullFeatured (unbalanced)", \
   "append-balanced.dat" using 1:3 with lines title "Tiny (balanced)", \
   "append-balanced.dat" using 1:5 with lines title "FullFeatured (balanced)"
set ylabel ""
unset grid
set origin 0.1, 0.7
set size 0.45,0.25
plot [4e6:] \
   "append.dat" using 1:2 with lines notitle, \
   "append.dat" using 1:4 with lines lt 2 notitle, \
   "append-balanced.dat" using 1:4 with lines lt 4 notitle
unset multiplot
unset ylabel

set grid
if (hardcopy) set output "get.png"; else set terminal wxt 2 raise
set multiplot layout 2,1 downwards
set key top left
set title "Min. unitary random get time"
plot \
   "get.dat" using 1:2 with linespoints title "Tiny (unbalanced)", \
   "get.dat" using 1:4 with linespoints title "FullFeatured (unbalanced)", \
   "get-balanced.dat" using 1:2 with lines title "Tiny (balanced)", \
   "get-balanced.dat" using 1:4 with lines title "FullFeatured (balanced)"
set key bottom right
set title ""
set ylabel "depth"
plot \
   "get.dat" using 1:3 with linespoints title "Tiny (unbalanced)", \
   "get.dat" using 1:5 with linespoints title "FullFeatured (unbalanced)", \
   "get-balanced.dat" using 1:3 with lines title "Tiny (balanced)", \
   "get-balanced.dat" using 1:5 with lines title "FullFeatured (balanced)", \
   log(x / 32.)/log(2.) with lines title "Optimal (|leaf|=32)"
unset multiplot
unset ylabel

if (hardcopy) set output "sub.png"; else set terminal wxt 3 raise
set multiplot layout 2,1 downwards
set key top left
set title "Min. unitary random sub time"
plot \
   "sub.dat" using 1:2 with linespoints title "Tiny (unbalanced)", \
   "sub.dat" using 1:4 with linespoints title "FullFeatured (unbalanced)", \
   "sub.dat" using 1:6 with lines title "Tiny (balanced)", \
   "sub.dat" using 1:8 with lines title "FullFeatured (balanced)"
set title ""
set ylabel "depth (average after sub)"
plot \
   "sub.dat" using 1:3 with linespoints title "Tiny (unbalanced)", \
   "sub.dat" using 1:5 with linespoints title "FullFeatured (unbalanced)", \
   "sub.dat" using 1:7 with lines title "Tiny (balanced)", \
   "sub.dat" using 1:9 with lines title "FullFeatured (balanced)"
unset multiplot
unset logscale x

# Local Variables: 
# mode: gnuplot
# End: 
