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
let rec catch matr nmatr x y w h k =
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
let detec matr nmatr w h =
let coef = ref 0 in
for y = 0 to h-1 do
  for x = 0 to w-1 do
    if matr.(x).(y) = 1 then
    begin
      incr coef;
      catch matr nmatr x y w h !coef
    end
  done
done;
!coef

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

let get_root vl connec =
let newmin = ref vl in
while !newmin > connec.(!newmin) do
  newmin := connec.(!newmin);
done;
!newmin

let get_min left top connec k =
let mini = ref 10000 in
if left > 0 then
  mini := get_root left connec;
if top > 0 then
  mini := min !mini (get_root top connec);
!mini

let set var connec mini =
let tmp = ref 0 and inc = ref var in
while connec.(!inc) < !inc do
  tmp := connec.(!inc);
  connec.(!inc) <- mini;
  inc := !tmp;
done;
connec.(!inc) <- mini

let set_min left top connec mini=
if left <> 0 then
  set left connec mini;
if top <> 0 then
  set top connec mini

let flattenL connec size =
let k = ref 1 in
for i = 1 to size - 1 do
  if connec.(i) < i then
    connec.(i) <- connec.(connec.(i))
  else
  begin
    connec.(i) <- !k;
    k := !k + 1
  end
done

let detecB matr nmatr connec w h =
let left = ref 0 and top = ref 0
   and mini = ref 0 and coef = ref 1 in
for y = 0 to h-1 do
  for x = 0 to w-1 do
    if matr.(x).(y) = 1 then
    begin
      left := nmatr.(x-1).(y);
      top := nmatr.(x).(y-1);
      mini := (get_min !left !top connec !coef);
     if !mini = 10000 then
     begin
       nmatr.(x).(y) <- !coef;
       connec.(!coef) <- !coef;
       coef := !coef + 1;
     end
     else
     begin
       nmatr.(x).(y) <- !mini;
       set_min !left !top connec !mini
     end
    end
  done
done;
flattenL connec !coef;
for y = 0 to h-1 do
  for x = 0 to w-1 do
    if nmatr.(x).(y) > 0 then
     nmatr.(x).(y) <- connec.(nmatr.(x).(y));
  done
done;
!coef


let supprim contbloc matr blocmatr w h max =
let deltaX = ref 0 and deltaY = ref 0 
  (*and bc = ref 1*) and dc = ref 1
    and tc = ref 1 and pred = ref 0
    and e = ref 0. in
for i = 0 to max do
  let (xmin,ymin,xmax,ymax) = contbloc.(i) in
  if xmax <> 0 then
  begin
    deltaX := xmax - xmin;
    deltaY := ymax - ymin;
    for y = ymin to ymax do
      for x = xmin to xmax do(*
        if blocmatr.(x).(y) = 1 then
          bc := !bc + 1;*)
        if matr.(x).(y) = 1 then
          dc := !dc + 1;
        if !pred = 0 && matr.(x).(y) = 1 then
        begin
          tc := !tc + 1;
          pred := 1;
        end;
        if !pred = 1 && matr.(x).(y) = 0 then
        begin
          tc := !tc + 1;
          pred := 0;
        end
      done
    done;
    if !deltaY < 130 && (((float)!dc) /. (float)!tc) < 7. then
      e := 0.
    else
    begin
    for y = ymin to ymax do
      for x = xmin to xmax do
        matr.(x).(y) <- 0;
      done
    done 
    end;
  e := 0.;
(*bc := 1;*)
  dc := 1;
  tc := 1;
  end;
done

let copymatr matr matrori w h =
for y = 0 to h -1 do
  for x = 0 to w -1 do
    matr.(x).(y) <- matrori.(x).(y);
  done
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
  let matrori = Array.make_matrix w h 255 in
  let hmatr = Array.make_matrix w h 0 in
  let vmatr = Array.make_matrix w h 0 in
  let resmatr = Array.make_matrix w h 0 in
  let fmatr = Array.make_matrix w h 0 in
  let blocmatr = Array.make_matrix w h 0 in
  let labelmatr = Array.make_matrix w h 0 in
  let charmatr = Array.make_matrix w h 0 in
  let finalmatr = Array.make_matrix w h 0 in
  let contmatrim = Array.make_matrix w h 0 in
  (*let max = ref 0 in*)
  let size = ref 0 in
  show img display;
  wait_key ();

  imag2matr img matra w h; 
  imag2matr img matrori w h;
  (*Premier passage hori*)
  hori_2 matra hmatr w h 300;
  (*Premier passage verti*)
  verti_2 matra vmatr w h 500;
  (*Combination des deux dernieres matrices*)
  matrand hmatr vmatr resmatr w h;
  (*Deuxiemem passage hori avec la combi des matr*)
  hori_2 resmatr blocmatr w h 30;
  matr2img dst blocmatr w h;
  show dst display;
  wait_key();
  
  (*Traitement des Images*)
  let connec = Array.make ((w*h)/2) 0 in
  size := detecB blocmatr labelmatr connec w h;
  let contbloc = Array.make (!size + 1) (w-1,h-1,0,0) in
  (*contour des bloc dans un Array*)
  charac contbloc labelmatr w h;
  (*créer les coutours sur charmatr*)
  arr2matr contbloc contmatrim w h (!size +1);
  (*Suppression des images*)
  supprim contbloc matrori blocmatr w h !size;
  matr2img dst2 matrori w h;
  show dst2 display;
  wait_key ();

  copymatr matra matrori w h;

  (*Traitement des textes*)
  (*Detect les coefs sur fmatr*)
  size := detec matrori fmatr w h;
  let cont = Array.make (!size +1)  (w-1,h-1,0,0) in
  (*contour des charac dans un Array*)
  charac cont fmatr w h;
  (*créer les coutonrs sur charmatr*)
  arr2matr cont charmatr w h (!size +1);
  (*Combination or du text et des cadres*)
  matror charmatr matra finalmatr w h;
  matr2img dst2 finalmatr w h;
  show dst2 display;
  wait_key ();
  Sdlvideo.save_BMP dst2 Sys.argv.(2);
    exit 0
  end

let _ = main ()
