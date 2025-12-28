# Makefile pour OCaml

# Nom de l'exécutable
EXEC = main

# Modules OCaml
MODULES = types lexer eval main

# Fichiers sources
ML = $(MODULES:=.ml)

# Fichiers objets
CMO = $(MODULES:=.cmo)

# Librairies
LIBS = str.cma

# Règle par défaut
all: $(EXEC)

# Compilation de l'exécutable
$(EXEC): $(CMO)
	ocamlc $(LIBS) -o $(EXEC) $(CMO)

# Compilation des fichiers objets
%.cmo: %.ml
	ocamlc -c $<

# Nettoyage
clean:
	rm -f *.cmo *.cmi $(EXEC)

.PHONY: all clean
