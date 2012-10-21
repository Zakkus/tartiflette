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

let dim surface =
  Sdlvideo.surface_dims surface

let mean_of_float x = match (modf x) with
  | (x,y) when (x >= 0.5)-> int_of_float(floor(y +. 1.))
  | (_,y) -> int_of_float(floor(y))

let center surface =
  let (width,height,pitch) = dim surface in
    match (width,height) with
      |(x,y) -> (x / 2,y / 2)

let hypotenuse surface =
  let (width,height,pitch) = Sdlvideo.surface_dims surface in
  let  hyp = sqrt(float_of_int(height*height + width*width)) in
   mean_of_float hyp

let tab width height = (Bigarray.Array2.create
                                Bigarray.int32
                                Bigarray.c_layout
                                width
                                height)

let img_to_matrice img =
 let (width,height,pitch) = Sdlvideo.surface_dims img
        in let my_matrice =
        (Bigarray.Array2.create
        Bigarray.int32
        Bigarray.c_layout
        width
        height) in
for j=0 to (height - 1) do
  for i=0 to (width - 1) do
   Bigarray.Array2.set my_matrice i j (Sdlvideo.get_pixel img i j)
 done;
 done;
my_matrice

let direct_int i j theta=
  let x = float_of_int i in
  let y = float_of_int j in
  let x' =    x*.cos(theta) -. y*.sin(theta) in
  let y' =    x*.sin(theta) +. y*.cos(theta) in
    (mean_of_float x', mean_of_float y')

let matrice_to_img matrice =
  let width = Bigarray.Array2.dim1 matrice in
  let height = Bigarray.Array2.dim2 matrice in
  let rm = Int32.of_int(255) in
  let gm = Int32.of_int(255) in
  let bm = Int32.of_int(255) in
  let am = Int32.of_int(255) in
  let my_img = Sdlvideo.create_RGB_surface
    [`SWSURFACE;`SRCCOLORKEY;`SRCALPHA]
    ~w:width
    ~h:height
    ~bpp:32
    ~rmask:rm
    ~gmask:gm
    ~bmask:bm
    ~amask:am in
for j=0 to (height - 1) do
for i=0 to (width - 1) do
  Sdlvideo.put_pixel my_img i j (Bigarray.Array2.get matrice i j)
done;
done;
my_img

let rotation img angle =
  let (height,width,pitch) = dim img in
  let (cy,cx) = center img in
  let cote_hypot = hypotenuse img  in
  let demi_hypot = cote_hypot / 2 in
  let my_output = tab cote_hypot cote_hypot in
  let my_input = img_to_matrice img in
    for i=0 to (width - 1) do
      for j=0 to (height - 1) do
        let (x,y) = direct_int (i - cx) (j - cy) angle in
            Bigarray.Array2.set
                my_output
                ( y + demi_hypot )
                ( x + demi_hypot )
                (Bigarray.Array2.get
                   my_input
                   (j)
                   (i));
      done;
    done;
  matrice_to_img my_output

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
  snd max

let main =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
      save (rotation img (hough img));
      (* on quitte *)
      exit 0
  end

let _= main()
