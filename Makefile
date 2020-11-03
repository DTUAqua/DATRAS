PACKAGE=DATRAS

all:
	make doc-update
	make build-package
	make install
	make pdf

doc-update:
	#echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"collate\", \"rd\"))" | R --slave
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",clean=TRUE)" | R --slave
	#rm $(PACKAGE)/NAMESPACE

build-package:
	R CMD build --resave-data=no $(PACKAGE)

install:
	R CMD INSTALL $(PACKAGE)

unexport TEXINPUTS
pdf:
	rm -f $(PACKAGE).pdf
	R CMD Rd2pdf --no-preview $(PACKAGE)

clean:
	rm -f DATRAS_1.0.tar.gz
	rm -f DATRAS.pdf

check:
	R CMD check $(PACKAGE)
