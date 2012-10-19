(*iDimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info
  img).Sdlvideo.h)

          (* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

          (* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
       Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

let show img dst =
  let d = Sdlvideo.display_format img in
  Sdlvideo.blit_surface d dst ();
  Sdlvideo.flip dst

let concat l1 l2 = match (l1,l2) with
	([],_) -> l2
	|(_,[]) -> l1
	|(e::l1,l2) -> e::concat l1 l2

let add_lign llign (a,b) = match llign with
	l::(c,d) when a = d -> concat l (c,a)
	|_ -> concat llign [(a,b)]

let compare_lign img h w y1 y2 =
	for x = 0 to w-1 do
        	if Sdlvideo.get_pixel img x  = 0 then
                begin   
                        y::lpix;
                        y <- y+1
                        x <- 0
                end
        done

let detect_area img h w llign lletter =
	let lpix = [] in
	for y = 0 to h-1 do
		for x = 0 to w-1 do
			if Sdlvideo.get_pixel img x y = 0 then
			begin
				concat lpix y;
				y <- y+1
				x <- 0
			end
		done
	done
	for i = 0 to length lpix - 2 do
		if (nth lpix i) + 1 = nth lpix i+1 then
		begin
			if llign = [] then concat llign [(nth lpix i, nth lpix i+1)]
			else llign = add_lign llign (nth lpix i, nth lpix i+1)
		end
	done
	

(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
  if Array.length (Sys.argv) < 2 then
    failwith "Il manque le nom du fichier!";

  (* Initialisation de SDL *)
  sdl_init ();
    (* Chargement d'une image *)
  let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
  let (w,h) = get_dims img in
  let matr = Array.make_matrix w h 0

