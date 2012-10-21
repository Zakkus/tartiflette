OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
ocr: rotation.ml
		${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr rotation.ml
		 
clean::
		rm -f *~ *.o *.cm? ocr
