Output.txt: Hack.hs
	stack runhaskell -- Hack.hs | tee Output.txt | less
