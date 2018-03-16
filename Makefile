Output.txt: Hack.hs Hack.o OneAToSwagger
	./OneAToSwagger | tee Output.txt | less

default: Hack.hs Hack.o
	stack ghc -- -package pipes-4.3.5 -package turtle -j3 --make Hack.hs -o OneAToSwagger

printOut:
	make default | lp

.PHONY: printOut
