(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(*Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

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

let img2matrix img =
  let (w,h) = get_dims img in
  let matrix = Array.make_matrix w h (255,255,255) in
    for j = 0 to h-1 do
      for i = 0 to w-1 do
	matrix.(i).(j) <- Sdlvideo.get_pixel_color img i j;
      done;
    done;
    matrix

let matrix2img matrix image w h =
  let img = Sdlvideo.create_RGB_surface_format image [] w h in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
        Sdlvideo.put_pixel_color img i j matrix.(i).(j);
      done;
    done;
  img

let hough matrix =
  let pi = acos(-1.) and pio2 = asin(1.) in
  let (w,h) = (Array.length matrix,Array.length matrix.(0)) in
  let d = int_of_float (sqrt (float_of_int (w*w+h*h))) in
  let matrixVote = Array.make_matrix d ((int_of_float (pi*.100.))+1) 0 in
  let t_max = ref 0. in
  let vote_max = ref 0 in
  for j = 0 to h-1 do
    for i = 0 to w-1 do
      if matrix.(i).(j) = (0,0,0) then
      begin
	let t = ref (-.pio2) in
	while !t <= pio2 do
	  let r = int_of_float ((float i *. (cos !t)) +. ((float j) *. (sin !t))) in
	  if r >= 0 then
	  begin
	    let t_i = int_of_float (!t*.100. +. pio2*.100.) in
	    matrixVote.(r).(t_i) <- matrixVote.(r).(t_i) +1;
	    if !vote_max < matrixVote.(r).(t_i) then
	    begin
	      vote_max := matrixVote.(r).(t_i);
	      t_max := !t;
	    end;
	  end;
	  t := !t +. 0.01
	done;
      end;
    done;
 done;
  (-.(!t_max)(*+.pio2*))

(* Rotation of one pixel *)
let rotate_pixel angle_cplx center pix =
        Complex.add center (
        Complex.mul angle_cplx (
        Complex.sub pix center))

(* Creation of a new matrix from a matrix *)
let make_nmat matrix w h =
        let diag = int_of_float (sqrt (float_of_int (w*w + h*h))) -1 in
        let (diff_w, diff_h) = (diag/2  - w /2 , diag/2 - h/2) in
        let nmat = Array.make_matrix  diag diag (0,0,0) in
        for x = 0 to w - 1 do
                for y = 0 to h - 1 do
                        nmat.(x+diff_w).(y+diff_h) <- matrix.(x).(y);
                done;
        done;
        (nmat, diag, diag, diff_w, diff_h, diag)

(* Rotation with an angle in radian *)
let rotate img angle  =
  let image = Sdlloader.load_image img in
  let w,h,z = Sdlvideo.surface_dims image in
  let matrix = Array.make_matrix w h (0,0,0) in
  if (Sdlvideo.must_lock image ) then
      Sdlvideo.lock image;
      for i = 0 to w-1 do
        for j = 0 to h-1 do
          matrix.(i).(j) <- Sdlvideo.get_pixel_color image i j;
        done ;
      done;
        let x = ref Complex.zero in
        let x' = ref Complex.zero in
        let (big_mat, w_g, h_g, diff_w, diff_h, diag)  =
                make_nmat matrix w h in
        let mat_rotate = Array.make_matrix diag diag (255,255,255) in
        let angle_cplx = {Complex.re=cos angle; Complex.im = sin angle} in                      (* Rotation angle in complex *)
        let center = {Complex.re=(float_of_int w_g) /.2.;Complex.im= (float_of_int h_g) /.2.} in    (* Image center in complex *)
        for i = diff_w to diff_w + w do
                for j = diff_h to diff_h + h do
                        x := {Complex.re=float_of_int i;Complex.im=float_of_int j};
                        x' := (rotate_pixel angle_cplx center !x);
                        mat_rotate.(int_of_float !x'.Complex.re).(int_of_float !x'.Complex.im) <- big_mat.(i).(j);
                done;
        done;
        matrix2img mat_rotate image w_g h_g

let main () =
  begin
    (* We want 1 argument *)
    if Array.length (Sys.argv) < 3 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation of SDL *)
    sdl_init ();
    (* Loading of an image *)
    let img = Sdlloader.load_image Sys.argv.(2) in
    (* Get dimensions *)
    let (w,h) = get_dims img in
    (* Creation of display surface in doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* Show image *)
      show img display;
      (* Wait key *)
      wait_key();
      (* Rotate and calcul the angle of the image *)
      let img_rotate = rotate Sys.argv.(1) (hough (img2matrix img)) in
      (* Show image *)
      show img_rotate display;
      (*Wait key *)
      wait_key();
      (* Save rotate image*)
      Sdlvideo.save_BMP img_rotate Sys.argv.(2);
      (* on quitte *)
      exit 0
  end

let _ = main ()
