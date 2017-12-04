# HC stands for haskell compiler, FLAGS stands for flags
HC = stack ghc
FLAGS = 

all: homeOwners

# Compiles Main.hs with flags
homeOwners:
	$(HC) Main.hs $(FLAGS)

# Removes all objects and binaries
clean:
	rm *.o *.hi Main
