lib:
	ocamlc -c ast.ml pprint.ml lexer.ml parser.ml util.ml

all: ti gmachine

ti: ast.cmo pprint.cmo lexer.cmo parser.cmo util.cmo
	ocamlc ast.cmo pprint.cmo lexer.cmo parser.cmo util.cmo ti.ml

gmachine: ast.cmo pprint.cmo lexer.cmo parser.cmo util.cmo
	ocamlc ast.cmo pprint.cmo lexer.cmo parser.cmo util.cmo gmachine.ml

utop:
	ocamlc -c ast.ml pprint.ml lexer.ml parser.ml util.ml
	ocamlc -a ast.cmo pprint.cmo lexer.cmo parser.cmo util.cmo -o lib.cma
	utop

clean:
	rm -f *.cmo *.cmi *.cma
