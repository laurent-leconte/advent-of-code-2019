let read_lines name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

(* turn a string of digits into a list of int *)
let int_list_of_str str =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) ((Char.code str.[i] - 48)::l) in
    exp (String.length str - 1) [] 

(* take a string and split it into layers of size width*height *)
let explode_layers data width height =
  let layer_size = width*height in
  let rec loop acc start_idx = 
    if start_idx + layer_size > String.length data then acc
    else
      let new_layer = int_list_of_str (String.sub data start_idx layer_size) in
      loop (new_layer::acc) (start_idx + layer_size) in
  List.rev (loop [] 0)      

let print_list l = print_endline (String.concat "," (List.map string_of_int l))

let rec count_pixels (x, y, z) = function
  | [] -> x,y,z
  | 0::tl -> count_pixels (x+1, y, z) tl
  | 1::tl -> count_pixels (x, y+1, z) tl
  | 2::tl -> count_pixels (x, y, z+1) tl
  | _::tl -> count_pixels (x, y, z) tl
  
let part1 layers =
  let layer_count = List.map (count_pixels (0,0,0)) layers in
  let _,y,z = List.hd (List.sort compare layer_count) in
  y*z
  
let rec transpose = function
  | [] 
  | [] :: _ -> []
  | rows    -> List.map List.hd rows :: transpose (List.map List.tl rows)

let rec first_visible_pixel = function
  | [] -> failwith "no visible pixel"
  | 0::_ -> 0
  | 1::_ -> 1
  | 2::tl -> first_visible_pixel tl

let string_of_pixels pix =
  let translate = function
    | 0 -> " "
    | 1 -> "#" in
  String.concat "" (List.map translate pix)

let display_pixels pix width height =
  let pixel_string = string_of_pixels pix in
  for i = 0 to (height - 1) do
    print_endline (String.sub pixel_string (i*width) width)
  done

let part2 layers =
  let pixel_stacks = transpose layers in
  let pixels = List.map first_visible_pixel pixel_stacks in
  display_pixels pixels 25 6

let data = List.hd (read_lines "day8.dat")
let layers = explode_layers data 25 6
let () = print_endline ("Part 1: " ^ (string_of_int (part1 layers)))
let () = part2 layers