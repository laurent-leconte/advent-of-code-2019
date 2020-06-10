(* to compile: `ocamlc unix.cma utils.ml` *)

let read_lines name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
  
let array_of_line line = 
  line |> String.split_on_char ';' |> List.map int_of_string |> Array.of_list
  
let read_program name =
  name |> read_lines |> List.hd |> array_of_line

(* read a list of coordinates (x,y tuples) and return its (xmin, xmax, ymin, ymax) bounding box *)  
let bounding_box coordinates =
  let union (xmin, xmax, ymin, ymax) (x,y) = (min xmin x, max xmax x, min ymin y, max ymax y) in
  let (x0, y0) = List.hd coordinates in
  List.fold_left union (x0, x0, y0, y0) coordinates

let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows)

(* take a sprite-to-string function and a canvas (a 2D-array of sprites);
  print the canvas out to stdin *)
let print_canvas string_of_sprite canvas = 
  let lists = Array.to_list (Array.map Array.to_list canvas) in
  let print_line l = 
    l |> List.map (string_of_sprite) |> String.concat "" |> print_endline in
  List.map print_line (List.rev (transpose lists))

(* read 1 char from the keyboard.
Taken from https://stackoverflow.com/questions/13410159/how-to-read-a-character-in-ocaml-without-a-return-key *)
let get1char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res
