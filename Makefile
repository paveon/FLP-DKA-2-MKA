GHC := ghc

SRC = $(wildcard src/*.hs)

all: dka-2-mka

dka-2-mka: $(SRC)
	$(GHC) $(GHCFLAGS) --make $(SRC) -o $@

pack:
	zip -r flp-fun-xpavel34.zip Makefile README src/*.hs test/

.PHONY: clean
clean:
	rm -f dka-2-mka ./src/*.o ./src/*.hi 2>/dev/null
