(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(*Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(*Sauvegarde de l'image*)
let save image = Sdlvideo.save_BMP image ("image rotate"^Sys.argv.(1))

(*
 *   show img dst
 *     affiche la surface img sur la surface 
 *     de destination dst (normalement
 *     l'écran)
 *     *)
let show img dst =
  let d = Sdlvideo.display_format img in
  Sdlvideo.blit_surface d dst ();
  Sdlvideo.flip dst

     (* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
       Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

let degree2radian theta =
  180. *. float_of_int theta /. 3.14159265359

let hough img =
  let (h,w) = get_dims img in
  let blackPixelList = ref [] in
    for i = 0 to w do
      for j = 0 to h do
	if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	  blackPixelList := (i,j)::!blackPixelList
      done;
    done;
  let d = int_of_float (sqrt (float_of_int (w*w + h*h))) in
  let matrix = Array.make_matrix d 61 0 in
    for p = 0 to List.length !blackPixelList do
      for teta = -30 to 30 do
	let rho = int_of_float (
		    (float_of_int (fst (List.nth !blackPixelList p)) *. cos (degree2radian theta))
		    +. (float_of_int (snd (List.nth !blackPixelList p)) *. sin (degree2radian theta))
		    ) in
	  if rho >= 0 then
	    matrix.(rho).(theta + 30) <- matrix.(rho).(theta + 30) + 1
      done;
    done;
  let max = (0,0) in
    for i = 1 to d - 1 do
      for j = 1 to 60 do
	if matrix.(fst max).(snd max) < matrix.(i).(j) then
	  max <- matrix.(i).(j)
      done;
    done;
  print_int snd max

let main =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (*(* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;*)
      (* on attend une touche *)
      wait_key();
      hough img;
      wait_key();
      (* on quitte *)
      exit 0
  end

let _= main()
