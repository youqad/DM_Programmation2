default: part1.byte part3.byte part2.byte

ALL = regexp.ml wregexp.ml dfa.ml test.ml
BYTE = $(ALL:.ml=.cmo)
OPT = $(ALL:.ml=.cmx)
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep

%.byte: %.ml $(BYTE)
	$(OCAMLC) $(BYTE) $< -o $@

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

-include .depend
.depend: $(wildcard *.ml *.mli)
	$(OCAMLDEP) $(wildcard *.ml *.mli) > .depend

part2.byte : part2.ml $(BYTE) part1.cmo
	ocamlc regexp.cmo wregexp.cmo dfa.cmo test.cmo part1.cmo $< -o $@

.PHONY: clean
clean:
	rm -f dm.* *.cmo *.cmi *.cmx *.o .depend
