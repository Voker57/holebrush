all: test

tests: test
	runghc tests.hs

test: src/Main.hs
	ghc -fno-warn-tabs -isrc --make src/Main.hs -o test
