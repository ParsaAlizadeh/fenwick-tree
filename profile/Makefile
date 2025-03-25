all: cpp-fen.exe hs-standalone.exe hs-library.exe
.PHONY: all

cpp-fen.exe: Fenwick.cpp
	g++ -O2 -g3 $< -o $@

hs-standalone.exe: Standalone.hs
	ghc -dynamic -O2 -g3 $< -o $@

hs-library.exe: Library.hs
	# cabal v2-build -O2 fenwick-tree
	# cabal v2-exec ghc -- -dynamic -O2 -g3 $< -o $@
	rm -rf /tmp/hs/Main.*
	ghc -i../src -outputdir/tmp/hs/ -dynamic -O2 -g3 $< -o $@
.PHONY: hs-library.exe

hs-library-v1.exe: LibraryV1.hs
	rm -rf /tmp/hs/Main.*
	ghc -i../src -outputdir/tmp/hs/ -dynamic -O2 -g3 $< -o $@
.PHONY: hs-library-v1.exe

hs-library-v2.exe: LibraryV2.hs
	rm -rf /tmp/hs/Main.*
	ghc -i../src -outputdir/tmp/hs/ -dynamic -O2 -g3 $< -o $@
.PHONY: hs-library-v2.exe

hs-library-v3.exe: LibraryV3.hs
	rm -rf /tmp/hs/Main.*
	ghc -i../src -outputdir/tmp/hs/ -dynamic -O2 -g3 $< -o $@
.PHONY: hs-library-v3.exe
