(*Fenêtre principale*)
let window =
  ignore(GMain.init ());
  let wnd = GWindow.window
    ~title:"Tartiflette"
    ~position:`CENTER
    ~decorated:true
    ~deletable:true
    ~resizable:true
    ~allow_grow:true
    ~allow_shrink:true
    ~width:800
    ~height:600 () in
  ignore (wnd#connect#destroy ~callback:GMain.quit);
  wnd
(* Fenêtre avec l'image traitée *)
let window2 =
  ignore (GMain.init ());
  let wnd = GWindow.window
    ~title:"Tartiflette"
    ~position:`NONE
    ~width:550
    ~height:600
    ~resizable:true () in
  ignore (wnd#event#connect#delete ~callback:(fun _ -> false));
  wnd

(* box *) 
let vbox2 = GPack.vbox
  ~homogeneous:false
  ~spacing:20
  ~border_width:15
  ~packing:window#add ()
let vbox = GPack.hbox
  ~homogeneous:false
  ~spacing:20
  ~border_width:15
  ~packing:window2#add ()

(*Toolbar*)
let toolbar = GButton.toolbar
  ~orientation:`HORIZONTAL
  ~style:`BOTH
  ~packing:vbox2#pack ()
(* Cadre de l'image *)
let image_box = GPack.hbox
  ~spacing:20
  ~border_width:15
  ~packing:vbox2#add ()

(*Buffer*) 
let buffer = GText.buffer ()
        let tview = GText.view 
			~buffer:buffer 
			~editable:false  
			~width:300
                        ~height:200
			~packing:vbox#add ()

(*Fonction insertion de texte*)
let insert_message message (buffer:GText.buffer) =  
  let iter = buffer#get_iter_at_char 0 in
  buffer#insert ~iter message;
   let start,stop = buffer#bounds in
   buffer#apply_tag_by_name "word_wrap" ~start ~stop ; 
   ()
(* balblabla424242 *)
  let default d = function
  | None -> d
  | Some v -> v

let file = ref "./default.png"
let file_origine = ref "./default.png"


(* afficheur *)
let image = GMisc.image
  ~file:!file
  ~width:300
  ~height:200
  ~packing:image_box#add ()


(* Bouton fermer*)
let close =
  let button = GButton.button
    ~label:"Exit"
    ~packing:toolbar#add () in
   ignore( button#connect#clicked ~callback: GMain.quit);
    button


(* Fenêtre de selection d'image *)
let ask_for_file parent _ =
  let dialog = GWindow.file_chooser_dialog 
    ~action:`OPEN
    ~title:"OPEN" 
    ~parent () in
    dialog#add_button_stock `CANCEL `CANCEL ;
    dialog#add_select_button_stock `OPEN `OPEN ;
    begin match dialog#run () with
      | `OPEN ->      
	  image#clear();
	  file := (default "<none>" dialog#filename);
	  file_origine := (default "<none>" dialog#filename);
	  image#clear ();
	  image#set_file (default "<none>" dialog#filename)
      | `DELETE_EVENT | `CANCEL -> ()
    end ;
    dialog#destroy ()

 (*Bouton ouvrir*)
let _open =
  let button = GButton.button
    ~label:"Open File"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (ask_for_file window));
    button

(*ZONE A MODIFIER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!: remplacer par les fonctions codées avant *)
let grey_array _ = Treatment2.image2grey !file 800 600 image;
  		file := ("./Imagetreated.bmp");
		image#clear ();
		image#set_file !file; ()

let noise _ = ; 		
		file := ("./Imagetreated.bmp");
		image#clear ();
		image#set_file !file; ()

let rotate _ = ;
		file := ("./Imagetreated.bmp");
		image#clear ();
		image#set_file !file; ()

let binarize _ = ;		
		file := ("./Imagetreated.bmp");
		image#clear ();
		image#set_file !file; ()

let line _ = ;
	     file := ("./Imagetreated.bmp");
             image#clear ();
             image#set_file !file; ()
let save _ = Treatment2.save !file ; ()
let processtext _ = 
     window2#show (); ()

let all _ =  grey_array () ; noise ()  ; binarize ();rotate (); line (); ()
 
(*Bouton niveau de gris*)
let grey =
  let button = GButton.button
    ~label:"Grey"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (grey_array));
    button

 (*Bouton noise*)
let noise =
  let button = GButton.button
    ~label:"Noise"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (noise));
    button

 (*Bouton rotate*)
let rotate =
  let button = GButton.button
    ~label:"Rotate"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (rotate));
    button

 (*Bouton binarisation*)
let binarization =
  let button = GButton.button
    ~label:"Binarization"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (binarize));
    button

(*Bouton All Treatment*)
let line2 =
  let button = GButton.button
    ~label:"Line"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (line));
    button 
 (*Bouton Process text*)
let text =
  let button = GButton.button
    ~label:"Process text"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (processtext));
    button

 (*Bouton sauvegarde*)
let save =
  let button = GButton.button
    ~label:"Save Image"
    ~packing:toolbar#add () in
 ignore(button#connect#clicked ~callback: (save));
    button

(*Nouveau main du projet*)
 let main () =
   begin
     Sdl.init [`VIDEO];
     Treatment.sdl_init ();
     at_exit Sdl.quit;
     window#show ();

     GMain.main();
     exit 0
   end
     
 let _ = main ()

