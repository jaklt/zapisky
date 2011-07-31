SOURCES= chapters/algorithms.tex \
         chapters/conclusion.tex \
		 chapters/documentation.tex \
		 chapters/implementation.tex \
		 chapters/introduction.tex \
		 chapters/literature.tex \
		 chapters/methodology.tex \
		 chapters/optimisations.tex \
		 chapters/results.tex \
		 graphs/graphs.plot

FIGURES= graphs/general.eps \
		 graphs/generalTuning.eps \
		 graphs/mctsTuning.eps \
		 graphs/memory.eps \
		 graphs/cores.eps

all: thesis.pdf

thesis.pdf: thesis.ps
	ps2pdf $< $@

thesis.ps: thesis.dvi
	dvips -o $@ -D600 -t a4 $<

${FIGURES}: graphs/graphs.plot
	cd graphs; gnuplot graphs.plot

thesis.dvi: thesis.tex ${SOURCES} $(wildcard *.tex) ${FIGURES}
	cslatex $<
	cslatex $<

SHELL = /usr/bin/env bash
clean:
	rm -f {chapters/*,*}.{log,dvi,aux,toc,lof,out} thesis.ps thesis.pdf
