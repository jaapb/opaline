OPTNESS?=	native
PREFIX?=	/usr/local

all: opaline

opaline: opaline.${OPTNESS}
	cp opaline.${OPTNESS} opaline

opaline.byte: opaline.ml
	ocamlbuild -use-ocamlfind $@

opaline.native: opaline.ml
	ocamlbuild -use-ocamlfind $@

install: opaline
	install -m 0755 opaline ${DESTDIR}${PREFIX}/bin
