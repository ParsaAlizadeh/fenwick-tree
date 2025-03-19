all: cpp-fen.exe hs-fen.exe
.PHONY: all

cpp-fen.exe: Fenwick.cpp
	g++ -O2 Fenwick.cpp -o cpp-fen.exe

hs-fen.exe: Main.hs
	ghc -dynamic -O2 Main.hs -o hs-fen.exe
