PROGRAM = tml

all : byte

test : byte
	ocamlrun $(PROGRAM).byte -t

native:
	ocamlbuild $(PROGRAM).native

byte:
	ocamlbuild $(PROGRAM).byte

clean:
	ocamlbuild -clean
