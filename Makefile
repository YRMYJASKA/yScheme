OUTPUT=main

all:
	ghc -o $(OUTPUT) --make main.hs
