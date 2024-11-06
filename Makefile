GHC = ghc
GHC_FLAGS=-package containers

SRC = BinTreeWorld.hs
EXE = BinTreeWorld

all: $(EXE)

$(EXE): $(SRC)
	$(GHC) $(GHC_FLAGS) -o $(EXE) $(SRC)

clean:
	rm -f $(EXE) *.o *.hi

