GHC        = ghc

src_files = src/AbsLatte.hs src/LexLatte.hs src/ParLatte.hs src/Frontend.hs src/MainLatte.hs src/PrintLatte.hs src/Backend.hs src/Midend.hs src/Optimizations.hs src/StringParsers.hs

.PHONY : all latc_llvm runtime clean

all: runtime latc_llvm

runtime: lib/runtime.c
	cd lib && clang -c -emit-llvm runtime.c -o runtime.bc

latc_llvm : $(src_files)
	${GHC} -o $@ $^

clean:
	-rm -f latc_llvm
	find . -type f \( -name "*.hi" -o -name "*.o" -o -name "*.log" -o -name "*.ll" -o -name "*.bc" \) -exec rm -f {} +