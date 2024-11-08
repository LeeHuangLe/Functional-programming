GHC = ghc
GHC_FLAGS=-dynamic

SRC = BinPacman.hs
EXE = BinPacman

all: $(EXE)

$(EXE): $(SRC)
	$(GHC) $(GHC_FLAGS) -o $(EXE) $(SRC)
	rm -f *.o *.hi

clean:
	rm -f $(EXE) *.o *.hi

