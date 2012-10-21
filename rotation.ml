(* Dimensions d'une image *)
let get_dims img =
  	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let save image = Sdlvideo.save_BMP image ("image rotate"^Sys.argv.(1)) 
 
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
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
 let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let tab width height = (Bigarray.Array2.create
                                Bigarray.int32
                                Bigarray.c_layout
                                width
                                height)

let shift_pixel input output ax ay bx by =
 Bigarray.Array2.set output bx by (Bigarray.Array2.get input ax ay)

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

let angle = ref 0.
let pi = 3.141592653
let rad = 180. /. pi
let deg = pi /. 180.
let degree_to_rad degree = float_of_int (degree) *. deg
let degreef_to_rad degree = degree *. deg

let create_angle a= angle := (degree_to_rad a)
let create_anglef a= angle := (degreef_to_rad a)

let dim surface =
  Sdlvideo.surface_dims surface

let foi x = float_of_int x
let iof x = int_of_float x
let soi x = string_of_int x
let sof x = string_of_float x
let abs x = abs_float x

let mean_of_float x = match (modf x) with
  | (x,y) when (x >= 0.5)-> int_of_float(floor(y +. 1.))
  | (_,y) -> int_of_float(floor(y))

let mean x = mean_of_float x

let direct_float x y theta=
  let x' =    x*.cos(theta) -. y*.sin(theta) in
  let y' =    x*.sin(theta) +. y*.cos(theta) in
    (mean_of_float x', mean_of_float y')

let direct_int i j theta=
  let x = float_of_int i in
  let y = float_of_int j in
  let x' =    x*.cos(theta) -. y*.sin(theta) in
  let y' =    x*.sin(theta) +. y*.cos(theta) in
    (mean_of_float x', mean_of_float y')

let inverse_float x y theta=
  let x' =    x*.cos(~-.theta) -. y*.sin(~-.theta) in
  let y' =    x*.sin(~-.theta) +. y*.cos(~-.theta) in
    (mean_of_float x',mean_of_float y')

let inverse_int i j theta=
  let x = float_of_int i in
  let y = float_of_int j in
  let x' =    x*.cos(~-.theta) -. y*.sin(~-.theta) in
  let y' =    x*.sin(~-.theta) +. y*.cos(~-.theta) in
    (mean_of_float x',mean_of_float y')

let hypotenuse surface =
  let (width,height,pitch) = Sdlvideo.surface_dims surface in
  let  hyp = sqrt(float_of_int(height*height + width*width)) in
   mean_of_float hyp

let center surface =
  let (width,height,pitch) = dim surface in
    match (width,height) with
      |(x,y) -> (x / 2,y / 2)


let is_in_rect x y width height =
  if (x > 0) && (x < width) && (y > 0) && (y <height) then
    true
  else
    false

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

let max_Hough_table hough_table =
  let cptMax = ref 0
  and thetaMax = ref 0. in
  let f (ro,theta) cpt =
    if !cptMax < cpt then
      begin
    cptMax := cpt;
    thetaMax := theta
      end
  in
    Hashtbl.iter f hough_table;
    !thetaMax

let getPix matrice x y =
  Bigarray.Array2.get matrice x y

(*let hough img =
  let pi = 3.14159 in
  let theta = ref (-15.) in
  let ro = ref 0 in
 let (height,width,_) = dim img in
  let hough_table = ref (Hashtbl.create ((width*height)/2)) in
    for x = 0 to height -1 do
      for y = 0 to width -1 do
	print_string ("before if");print_int(x);print_string(" "); print_int(y);
    if ((getPix (img_to_matrice img) x y) = Int32.of_int(0)) then
	begin
      print_string("after if");
	while (!theta<16.) do

          let radtheta = !theta/.180.0*.pi in
            begin
	  print_string("before ro");

          ro := int_of_float((float x)*.cos(radtheta) +.
                               (float y)*.sin(radtheta));
	  print_string("ro done");
         try
	    print_string("before cpt");
            let cpt = Hashtbl.find !hough_table (!ro,radtheta) in
              Hashtbl.replace !hough_table (!ro,radtheta) (cpt+1);
	    print_string("cpt done!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
          with
            |Not_found -> Hashtbl.add !hough_table (!ro,radtheta) 1;
            end;
	print_string("before thetaOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOoo");
          theta := !theta +. 1.0;
	print_string("after theta3333333333333333333333333333");
   	done;
	end;
	print_string("almost dooooooon6666666666666666666");
	theta := -15.;
   	done;
    done;
    print_float (max_Hough_table !hough_table)*)

let max_tab tab height width =
let max = ref 0 in
let thetamax = ref 0 in 
	for r=0 to height -1 do
		for theta=0 to width -1 do
		if tab.(r).(theta) > !max then
		max:= tab.(r).(theta);thetamax := theta;
		done;
	done;
		!thetamax
			

let hough img = 
	let (height,width) = get_dims img in
	let pi = 3.14159265 in
	let hough_table = Array.make_matrix ((iof(sqrt(foi(height)*.foi(height)+.foi(width)*.foi(width))))) 361 0 in
	for i=0 to height -1 do
		for j=0 to width -1 do
				print_string("***");print_int(j);print_string("***");
				for theta = (-15) to 15 do
				let r = (foi j)*.cos(foi theta)+.(foi i)*.sin(foi theta) in
					if(r>=0.) then
					begin
					hough_table.(iof r).(( theta)+15) <- hough_table.(iof r).((theta)+15) + 1;
					print_float(r);print_string(" ");print_int(i);print_string(" "); print_int(j);print_string(" ");
					end
				done;
		done;
	done;
		 let thetamax = max_tab hough_table (iof(sqrt(foi(height)*.foi(height)+.foi(width)*.foi(width)))) 361  in
		(*pi*.(foi thetamax)/.180.0;*)
			print_string("thethamax::");  
			print_float(pi*.(foi thetamax)/.180.0);
			save img

	
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
    (* On crée la surface d'affichage en doublebuffering *)
    (*let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in*)
      (* on affiche l'image *)
      (*show img display;*)
      (* on attend une touche *)
      hough img;
      wait_key();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()
