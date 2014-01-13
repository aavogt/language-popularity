
SHELL := bash

default: analyze.Rmd cabalf.hs
	R --no-save <<< "library(knitr); knit('analyze.Rmd'); pandoc('analyze.md')"

.PHONY : clean
clean:
	rm -rf cabalf{,.{hi,o}} analyze.md cache 
