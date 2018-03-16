Output.txt: default
	./OneAToSwagger | tee Output.txt | less

default: Hack.hs
	stack ghc -- -package pipes-4.3.5 -package turtle -j3 --make Hack.hs -o OneAToSwagger

.PHONY: default
