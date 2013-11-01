
.PHONY: all
all: logo

logo: Main.hs SVG.hs Eval.hs Parse.hs ForwardLogo.hs RightLogo.hs RepeatLogo.hs	
	ghc -o logo Main.hs

.PHONY: tidy
tidy:
	rm -f Main.hi SVG.hi Eval.hi Parse.hi ForwardLogo.hi RightLogo.hi RepeatLogo.hi
	rm -f Main.o  SVG.o  Eval.o  Parse.o  ForwardLogo.o  RightLogo.o  RepeatLogo.o

.PHONY: clean
clean: tidy
	rm -f logo
