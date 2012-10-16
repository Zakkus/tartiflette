(* Dimensions d'une image *)
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

let first (a,b,c) = a
let trip a = (a,a,a)

(* Calcul du niveau de gris d'un pixel *)
let level (r,g,b) =
  (int_of_float)(0.299*.(float)r +. 0.587*.(float)g +. 0.114*.(float)b)

(* Mise à niveau du gris dans la matrice *)
let image2grey src h w matr=
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      let tmp = Sdlvideo.get_pixel_color src x y in
      matr.(x).(y) <- first(trip(level tmp));
    done
  done


(* Création de la surface
   à partir de la matrice *)
let modsrf matr srf h w =
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
    Sdlvideo.put_pixel_color srf x y (trip(matr.(x).(y)))
    done
  done

(* Fonction de comparaison
   pour le tri des Array *)
let comp a = function
  b when a = b -> 0
 |b when a < b -> -1
 |_ -> 1

(*Supression du bruit dans la matrice *)
let noiseOut dst h w matr=
  let median = Array.make 9 0 in
    for y = 1 to h -2 do
      for x = 1 to w - 2 do
        median.(0) <- (matr.(x-1).(y-1));
	median.(1) <- (matr.(x).(y-1));
	median.(2) <- (matr.(x+1).(y-1));
	median.(3) <- (matr.(x-1).(y));
	median.(4) <- (matr.(x).(y));
	median.(5) <- (matr.(x+1).(y));
	median.(6) <- (matr.(x-1).(y+1));
	median.(7) <- (matr.(x).(y+1));
	median.(8) <- (matr.(x+1).(y+1));

	Array.fast_sort comp median;
	matr.(x).(y) <- median.(4)
      done
    done

let minimum a b c d e f j i current =
 min a
 (min b
 (min c
 (min d
 (min e
 (min f
 (min j
 (min i current
 )))))))

let maximum a b c d e f j i current =
 max a
 (max b
 (max c
 (max d
 (max e
 (max f
 (max j
 (max i current
 )))))))

let binar src dest h w =
    for y = 1 to h -2 do
      for x = 1 to w - 2 do
      let a = first (Sdlvideo.get_pixel_color src (x-1) (y-1)) in
      let b = first (Sdlvideo.get_pixel_color src (x-1) (y)) in
      let c = first (Sdlvideo.get_pixel_color src (x-1) (y+1)) in
      let d = first (Sdlvideo.get_pixel_color src (x) (y-1)) in
      let e = first (Sdlvideo.get_pixel_color src (x) (y+1)) in
      let f = first (Sdlvideo.get_pixel_color src (x+1) (y-1)) in
      let i = first (Sdlvideo.get_pixel_color src (x+1) (y)) in
      let j = first (Sdlvideo.get_pixel_color src (x+1) (y+1)) in 
      let current = first (Sdlvideo.get_pixel_color src x y) in
      let min = minimum a b c d e f i j current in
      let max = maximum a b c d e f i j current in
      if 127 > ((min + max)/2) then
        Sdlvideo.put_pixel_color dest x y (0,0,0)
      else
	Sdlvideo.put_pixel_color dest x y (255,255,255)
    done
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
    let matr = Array.make_matrix w h 0 in
    (* On crée la surface d'affichage en doublebuffering *)
  let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
  (*niveau de gris*)
  let ndisp = Sdlvideo.create_RGB_surface_format img [] w h in 
  (*bruit*)
  let bdisp = Sdlvideo.create_RGB_surface_format img [] w h in 
 
 (*binarisation*)
  let binardisp = Sdlvideo.create_RGB_surface_format img [] w h in 


  (*creMatr1 img matrO w h;*)
  (* on affiche l'image *)
   show img display;
  (* on attend une touche *)
  wait_key ();
  
  image2grey img h w matr; (*modif de la matrice *)
  modsrf matr ndisp h w;(*création de la surface*)
  show ndisp display;(*affichage*)
  wait_key ();

  noiseOut bdisp h w matr;(*modif matrice*)
  modsrf matr bdisp h w;(*création de la surface*)
  show bdisp display;(*affichage*)
  wait_key();


  binar bdisp binardisp h w;
  show binardisp display;
  wait_key ();




      (* on quitte *)
   exit 0
  end


let _ = main ()
