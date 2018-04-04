OPTNESS=	native

opaline: opaline.$(OPTNESS)
	cp opaline.$(OPTNESS) opaline

opaline.byte: opaline.ml
	ocamlbuild -use-ocamlfind $@

opaline.native: opaline.ml
	ocamlbuild -use-ocamlfind $@
