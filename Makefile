OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
tpsdl: traitement.ml
		${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr traitement.ml
		 
clean::
		rm -f *~ *.o *.cm? ocr
