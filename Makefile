OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
ocr: treatment2.ml
		${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr treatment2.ml
		 
clean::
		rm -f *~ *.o *.cm? ocr
