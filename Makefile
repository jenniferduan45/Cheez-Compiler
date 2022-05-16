
.PHONY: all test clean

# test all testcases
test:
# ./cheez.native < ./test/test_1_ops.cheez > example.out && lli example.out
	python3 test.py

all: cheez.native lib.o
# cheez IR
cheez.native:
	# opam config exec -- \
	opam exec
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis cheez.native
	# ocamlbuild -pkgs llvm,llvm.analysis cheez.native

	cc -c chezlib.c

# external lib


# temporary testing, ir and sast, for testing purpose
temp:
	make all && ./cheez.native < temp.cheez > temp.out && lli temp.out

temp-i:
	make all && ./cheez.native < temp.cheez
temp-s:
	make all && ./cheez.native -s < temp.cheez
build:
	ocamlbuild -clean
	ocamlbuild cheez.native -pkgs llvm,llvm.analysis


# print out states of current grammar
checkgrammar:
	ocamlyacc -v parser.mly


# clean up
clean:
	ocamlbuild -clean
	rm -rf *.native *.out *.o ./test/*.out
