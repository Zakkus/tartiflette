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

(*Converti une image en matrice*)
let imag2matr img matr w h =
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      if  Sdlvideo.get_pixel_color img x y = (0,0,0) then
	matr.(x).(y) <- 1
      else
	matr.(x).(y) <- 0
    done
  done

(*Converti une matrice en image*)
let matr2img srf matr w h =
  for y = 0 to h -1 do
    for x = 0 to w-1 do
     if matr.(x).(y) = 1 then
     Sdlvideo.put_pixel_color srf x y (0,0,0)
     else
     Sdlvideo.put_pixel_color srf x y (255,255,255)
    done
  done
(*
let hori matr nmatr w h avrg =
let time = ref 0 in
  for y = 0 to h-1 do
    for x = 0 to w-1 do
     if matr.(x).(y) = 0 then
     begin
       time := !time + 1;
       nmatr.(x).(y) <- 0;
     end
     else
     begin
       if (!time < avrg) then
begin 
         for i = x - !time to x do
           nmatr.(i).(y) <- 1;
         done;
     end;  
       nmatr.(x).(y) <- 1;
       time := 0;
    end;
    done
  done

let verti matr nmatr w h avrg =
let time = ref 0 in
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      if matr.(x).(y) = 0 then
      begin
      incr time;
      nmatr.(x).(y) <- 0;
      end
      else
      begin
        if !time < avrg then
          for i = y - !time to y do
            nmatr.(x).(i) <- 1;
          done;

        nmatr.(x).(y) <- 1;
        time := 0
      end
    done;
   time := 0;
  done
*)
(*remplissage hori*)
let hori_2 mtx mtx2 w h c =
let n = ref 0 and min = ref 0 in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
        if mtx.(x).(y) = 0 then
        begin
          mtx2.(x).(y) <- 0;
          n := !n+1
        end
        else
        begin
          if !min = 0 then
          begin
            min := x+1;
            n := 0;
            mtx2.(x).(y) <- 1
          end
          else
            if !n <= c then
            begin
              for i = !min to x+1 do
                mtx2.(i).(y) <- 1
              done;
              min := x+1;
              n := 0;
            end
            else
            begin
              mtx2.(x).(y) <- 1;
              min := x+1;
              n := 0
            end
          end
      done;
      min := 0;
      n := 0;
    done

(*remplissage verti*)
let verti_2 mtx mtx2 w h c =
let n = ref 0 and min = ref 0 in
    for x = 0 to w-1 do
      for y = 0 to h-1 do
        if mtx.(x).(y) = 0 then
        begin
          mtx2.(x).(y) <- 0;
          n := !n+1
        end
        else
        begin
          if !min = 0 then
          begin
            min := y+1;
            n := 0;
            mtx2.(x).(y) <- 1
          end
          else
            if !n <= c then
            begin
              for i = !min to y+1 do
                mtx2.(x).(i) <- 1
              done;
              min := y+1;
              n := 0;
            end
            else
            begin
              mtx2.(x).(y) <- 1;
              min := y+1;
              n := 0
            end
          end
      done;
      min := 0;
      n := 0;
    done

(*And logique de deux matrices*)
let matrand hmatr vmatr resmatr w h =
let a = ref 0 in 
let b = ref 0 in
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      a := hmatr.(x).(y);
      b := vmatr.(x).(y);
      if a = b then
        resmatr.(x).(y) <- !a
      else
        resmatr.(x).(y) <- 0
    done
  done

(*Or logique de deux matrices*)
let matror hmatr vmatr resmatr w h =
let a = ref 0 in 
let b = ref 0 in
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      a := hmatr.(x).(y);
      b := vmatr.(x).(y);
      if a = b then
        resmatr.(x).(y) <- !a
      else
        resmatr.(x).(y) <- 1
    done
  done

(*fonction recurssives sur tous les pixels de la lettre*)
let rec catch matr nmatr x y w h k=
begin
  nmatr.(x).(y) <- k;
  matr.(x).(y) <- 0;
  if (x-1 > -1) && matr.(x-1).(y) = 1 then
      catch matr nmatr (x-1) y w h k;
  if x+1 < w && matr.(x+1).(y) = 1 then
      catch matr nmatr (x+1) y w h k;
  if y-1 > -1 && matr.(x).(y-1) = 1 then
      catch matr nmatr x (y-1) w h k;
  if y+1 < h && matr.(x).(y+1) = 1 then
      catch matr nmatr x (y+1) w h k;
end

(*parcours de l'image pour detecter chaque nouvelle lettre*)
let detec matr nmatr w h size =
let coef = ref 0 in
for y = 0 to h-1 do
  for x = 0 to w-1 do
    if matr.(x).(y) = 1 then
    begin
      incr coef;
      catch matr nmatr x y w h !coef;
    end
  done
done;
size := !coef

(*Parcours l'Array pour déterminer les quatres coord
extremes de chaque lettres*)
let charac arr omatr w h =
for y = 0 to h -1 do
  for x = 0 to w-1 do
    if omatr.(x).(y) > 0 then
    begin
      let (a,b,c,d) = arr.(omatr.(x).(y)) in
	if (a > x) then
        arr.(omatr.(x).(y)) <- (x,b,c,d);
	if (x > c) then
	arr.(omatr.(x).(y)) <- (a,b,x,d);
	if (b > y) then
	arr.(omatr.(x).(y)) <- (a,y,c,d);
	if (d < y) then
	arr.(omatr.(x).(y)) <- (a,b,c,y);
    end;
  done
done


(*Transforme l'array en matrice : dessine le cadre
autour de chaque lettre*)
let arr2matr arr matr w h max=
for l = 0 to max-1 do
  let (xmin,ymin,xmax,ymax) = arr.(l) in
  for i = xmin to xmax do
    matr.(i).(ymin) <- 1;
    matr.(i).(ymax) <- 1;
  done;
  for j = ymin to ymax do
    matr.(xmin).(j) <- 1;
    matr.(xmax).(j) <- 1;
  done;
done

(* main *)
let main () =
  begin
    (* Nous voulons 2 argument *)
  if Array.length (Sys.argv) < 3 then
    failwith "Il manque le nom du fichier!";

  (* Initialisation de SDL *)
  sdl_init ();
    (* Chargement d'une image *)
  let img = Sdlloader.load_image Sys.argv.(2) in
  (* On récupère les dimensions *)
  let (w,h) = get_dims img in
  (* On crée la surface d'affichage en doublebuffering *)
  let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
  (*let matr = Array.make_matrix w h 0 in*)
  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
  let dst2 = Sdlvideo.create_RGB_surface_format img [] w h in
  let matra = Array.make_matrix w h 255 in
  let matrb = Array.make_matrix w h 255 in
  let hmatr = Array.make_matrix w h 0 in
  let vmatr = Array.make_matrix w h 0 in
  let resmatr = Array.make_matrix w h 0 in
  let fmatr = Array.make_matrix w h 0 in
  let lastmatr = Array.make_matrix w h 0 in
  let charmatr = Array.make_matrix w h 0 in
  let finalmatr = Array.make_matrix w h 0 in
  let size = ref 0 in
  show img display;
  wait_key ();

  imag2matr img matra w h; 
  imag2matr img matrb w h;
  (*Premier passage hori*)
  hori_2 matra hmatr w h 300;
  (*Premier passage verti*)
  verti_2 matra vmatr w h 500;
  (*Combination des deux dernieres matrices*)
  matrand hmatr vmatr resmatr w h;
  (*Deuxiemem passage hori avec la combi des matr*)
  hori_2 resmatr lastmatr w h 30;
  matr2img dst lastmatr w h;
  show dst display;
  wait_key();

  (* detect les coefs sur fmatr*)
  detec matra fmatr w h size;
  let cont = Array.make (!size +1)  (w-1,h-1,0,0) in
  (*contour des charac dans un Array*)
  charac cont fmatr w h;
  (*créer les coutonrs sur charmatr*)
  arr2matr cont charmatr w h (!size +1);
  (*Combination or du text et des cadres*)
  matror charmatr matrb finalmatr w h;
  matr2img dst2 finalmatr w h;
  show dst2 display;
  wait_key ();
  Sdlvideo.save_BMP dst2 Sys.argv.(2);
    exit 0
  end

let _ = main ()
