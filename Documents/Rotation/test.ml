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

let pi = 3.141592653
let rad = 180. /. pi
let deg = pi /. 180.
let degree_to_rad degree = float_of_int (degree) *. deg
let degreef_to_rad degree = degree *. deg

let angle = ref 0.
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

let rotation img angle=
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
