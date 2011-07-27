SOURCES= chapters/algorithms.tex \
         chapters/conclusion.tex \
		 chapters/documentation.tex \
		 chapters/implementation.tex \
		 chapters/introduction.tex \
		 chapters/literature.tex \
		 chapters/methodology.tex \
		 chapters/optimisations.tex

all: thesis.pdf

thesis.pdf: thesis.ps
	ps2pdf $< $@

thesis.ps: thesis.dvi
	dvips -o $@ -D600 -t a4 $<

thesis.dvi: thesis.tex ${SOURCES} $(wildcard *.tex)
	cslatex $<
	cslatex $<

SHELL = /usr/bin/env bash
clean:
	rm -f {chapters/*,*}.{log,dvi,aux,toc,lof,out} thesis.ps thesis.pdf
