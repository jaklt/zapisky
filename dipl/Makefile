all:
	pdflatex thesis.tex
	makeindex thesis
	bibtex thesis
	pdflatex thesis.tex
	pdflatex thesis.tex

clean:
	rm -f thesis.aux thesis.bbl thesis.blg thesis.idx thesis.ilg thesis.ind thesis.log thesis.toc thesis.out
