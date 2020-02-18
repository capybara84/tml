PROGRAM = tml

all : byte

test : test
	ocamlrun $(PROGRAM).byte -t

native:
	ocamlbuild $(PROGRAM).native

byte:
	ocamlbuild $(PROGRAM).byte

clan:
	ocamlbuild -clean
