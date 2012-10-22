OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
OCAMLDPD =  treatment2.ml rotation.ml detection.ml main.ml

ocr: main.ml treatment2.ml rotation.ml detection.ml
		${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o tartiflette ${OCAMLDPD}
clean::
		rm -f *~ *.o *.cm? ocr
