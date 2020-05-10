#load "intercom.cmo";;
#load "utils.cmo";;

type tile = {
  x: int;
  y: int;
  id: int
}

let string_of_tile_id = function
  | 0 -> " "
  | 1 -> "X"
  | 2 -> "#"
  | 3 -> "="
  | 4 -> "o"
  | n -> failwith (Printf.sprintf "Unkown tile id %d" n)

let rec tiles_of_list = function
  | [] -> []
  | x::y::id::tl -> {x; y; id} :: (tiles_of_list tl)
  | _ -> failwith "incorrect format"

(* read and execute program and parse the output (don't forget to reverse the list first) *)
let tiles = (Utils.read_program "day13.dat", [])
  |> Intercom.init_program
  |> Intercom.execute
  |> Intercom.get_output
  |> List.rev
  |> tiles_of_list
  
(* build a 2D array with the content of the tiles *)
let paint_screen tiles =
  (* get the tiles' bounding box *)
  let xmin, xmax, ymin, ymax = tiles 
    |> List.map (fun (t:tile) -> (t.x, t.y))
    |> Utils.bounding_box in
  (* create empty screen *)
  let screen = Array.make_matrix (xmax + 1) (ymax + 1) 0 in
  let paint_one t = screen.(t.x).(t.y) <- t.id; in
  let _ = List.map paint_one tiles in
  screen

let _ = tiles
 |> paint_screen
 |> Utils.print_canvas string_of_tile_id

let filter acc {x; y; id} =
  match id with
    | 2 -> acc + 1
    | _ -> acc
    
let () = tiles
  |> List.fold_left filter 0
  |> print_int