
set terminal png size 650,800
set grid
set key left

set logscale x
set output "append.png"
set multiplot layout 2,1 downwards
set title "Min. unitary append time"
plot \
   "append.dat" using 1:2 with linespoints title "Tiny (unbalanced)", \
   "append.dat" using 1:4 with linespoints title "FullFeatured (unbalanced)", \
   "append-balanced.dat" using 1:2 with lines title "Tiny (balanced)", \
   "append-balanced.dat" using 1:4 with lines title "FullFeatured (balanced)"
set title ""
set ylabel "depth (after appends)"
plot \
   "append.dat" using 1:3 with linespoints title "Tiny (unbalanced)", \
   "append.dat" using 1:5 with linespoints title "FullFeatured (unbalanced)", \
   "append-balanced.dat" using 1:3 with lines title "Tiny (balanced)", \
   "append-balanced.dat" using 1:5 with lines title "FullFeatured (balanced)"
unset multiplot
unset ylabel

set output "get.png"
set multiplot layout 2,1 downwards
set title "Min. unitary random get time"
plot \
   "get.dat" using 1:2 with linespoints title "Tiny (unbalanced)", \
   "get.dat" using 1:4 with linespoints title "FullFeatured (unbalanced)", \
   "get-balanced.dat" using 1:2 with lines title "Tiny (balanced)", \
   "get-balanced.dat" using 1:4 with lines title "FullFeatured (balanced)"
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

set output "sub.png"
set multiplot layout 2,1 downwards
set title "Min. unitary random sub time"
plot \
   "sub.dat" using 1:2 with linespoints title "Tiny (unbalanced)", \
   "sub.dat" using 1:4 with linespoints title "FullFeatured (unbalanced)", \
   "sub.dat" using 1:6 with lines title "Tiny (balanced)", \
   "sub.dat" using 1:8 with lines title "FullFeatured (balanced)"
set title ""
set ylabel "depth"
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
