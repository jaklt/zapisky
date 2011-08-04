set term postscript eps enhanced

set xtics (3,10,30)
set xrange [0:33]
set format y "%2.0f%%"
set format x "%2.0fs"

set output "generalTuning.eps"

plot "./data.dat" using 1:2 title "full" \
		with linespoints linetype 1 linewidth 2      \
		     pointtype 8, \
	"./data.dat" using 1:4 title "noTT"  \
		with linespoints linetype 7 linewidth 2,     \
	"./data.dat" using 1:5 title "EVAL=fairy" \
		with linespoints linetype 3 linewidth 2 \
		     pointtype 5, \
	"./data.dat" using 1:7 title "goal check" \
		with linespoints linetype 5 linewidth 2

set output "mctsTuning.eps"

plot "./data.dat" using 1:2 title "full" \
		with linespoints linetype 1 linewidth 2      \
		     pointtype 8, \
	"./data.dat" using 1:3 title "noHH"  \
		with linespoints linetype 7 linewidth 2,     \
	"./data.dat" using 1:6 title "noHeavyPlayout" \
		with linespoints linetype 3 linewidth 2 \
		     pointtype 5

set xtics (3,5,10,15,30)
set xrange [0:33]

set output "general.eps"

plot "./general.dat" using 1:2 title "full" \
		with linespoints linetype 7 linewidth 2

#####################################################
set nokey

set xtics (1,2,4,8)
set xrange [0:9]
set format y "{/Times=25 %2.0f%%}"
set format x "{/Times=25 %2.0f}"

set output "cores.eps"
set title "{/Times=25 CPU's}"

plot "./cores.dat" with linespoints linetype 1 linewidth 8 \
		pointtype 7 pointsize 3

set xtics (100,200,400)
set xrange [90:410]
set format y "{/Times=25 %2.1f%%}"
set format x "{/Times=25 %2.0fMB}"

set output "memory.eps"
set title "{/Times=25 Memory size}"

plot "./memory.dat" with linespoints linetype 1 linewidth 8 \
		pointtype 7 pointsize 3
