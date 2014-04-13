
all: RTree.hs
	cabal build --ghc-options="-rtsopts -threaded"

