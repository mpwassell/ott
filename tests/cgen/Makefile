OTT := "../../bin/ott"
OCAMLBUILD := ocamlbuild  $(CFLAGS)  -use-ocamlfind -package crowbar -cflag -annot -tag debug

all: main_test01 main_test02

test01.ml: test01.ott 
	$(OTT) -i test01.ott -o test01.ml -cgen test01_cgen.ml

main_test01: test01.ml test01_cgen.ml
	$(OCAMLBUILD) main_test01.native

test02.ml: test02.ott 
	$(OTT) -i test02.ott -o test02.ml -cgen test02_cgen.ml

main_test02: test02.ml test02_cgen.ml
	$(OCAMLBUILD) main_test02.native
