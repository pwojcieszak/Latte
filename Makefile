LATTE_CF = Latte.cf
SRC_DIR = src

.PHONY : all clean

all: bnfc runtime

bnfc: $(SRC_DIR)$(LATTE_CF)
	cd $(SRC_DIR) && bnfc --functor -m $(LATTE_CF)

runtime: lib/runtime.c
	cd lib && clang -c -emit-llvm runtime.c -o runtime.bc

clean:
	-rm -f latc_llvm
	find . -type f \( -name "*.hi" -o -name "*.o" -o -name "*.log" -o -name "*.ll" -o -name "*.bc" \) -exec rm -f {} +