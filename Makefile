GHC        = ghc
LATTE_CF = Latte.cf
SRC_DIR = src/

src_files = src/AbsLatte.hs src/LexLatte.hs src/ParLatte.hs src/MySkelLatte.hs src/MyTestLatte.hs

.PHONY : all latc_llvm clean

all: runtime latc_llvm #pliki My... i kopiowanie do wygenerowanych przez bnfc i potem dopiero make parsera

runtime: lib/runtime.c
	cd lib && clang -c -emit-llvm runtime.c -o runtime.bc

latc_llvm : $(src_files)
	${GHC} -o $@ $^

clean:
	-rm -f latc_llvm
	find . -type f \( -name "*.hi" -o -name "*.o" -o -name "*.log" -o -name "*.ll" -o -name "*.bc" \) -exec rm -f {} +