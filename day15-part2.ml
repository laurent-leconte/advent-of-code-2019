#load "utils.cmo";;

type point = int*int

type graph = {
  mutable origin: point;
  neighbors: (point, point list) Hashtbl.t;
  distance: (point, int) Hashtbl.t;
}

let tile_of_char = function
  | '~' -> -1 (* unexplored tile. Should not happen *)
  | '#' -> 0  (* wall *) 
  | ' ' -> 1  (* empty space *)
  | 'o' -> 2  (* oxygen system - origin of the graph *)
  | _   -> 3


let load_input file =
  let lines = Utils.read_lines file in
  let m, n = (List.length lines, String.length @@ List.hd lines) in
  Printf.printf "Maze dimensions : %d, %d\n" m n;
  let result = Array.make_matrix m n (-1) in
  (* iterate over a line: split string, update result Array *)
  let iter_line i line =
    let iter_char j c =
      result.(j).(i) <- tile_of_char c in
    String.iteri iter_char line in
  (* iterate over lines *)
  List.iteri iter_line lines;
  result

let build_graph maze =
  let n = Array.length maze in (* assume square maze because I'm lazy *)
  let new_graph = {
    origin = (0, 0);
    neighbors = Hashtbl.create (n*n);
    distance = Hashtbl.create (n*n);
  } in
  let adjacent i j = [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] in
  let is_open (i, j) =
    match maze.(i).(j) with
      | 0 -> None
      | _ -> Some (i, j) in
  let neighbors i j = List.filter_map is_open (adjacent i j) in
  for i = 1 to (n - 2) do
    for j = 1 to (n - 2) do
      Hashtbl.add new_graph.neighbors (i, j) (neighbors i j);
      (* Hashtbl.add new_graph.distance (i, j) (-1); *)
      if maze.(i).(j) = 2 then new_graph.origin <- (i,j);
    done
  done;
  new_graph
      
let compute_distances graph =
  let unexplored_neighbors node =
    let is_not_seen n =
      match Hashtbl.find_opt graph.distance n with
        | None -> Some n
        | Some _ -> None in
    List.filter_map is_not_seen (Hashtbl.find graph.neighbors node) in 
  let rec one_step dist = function
    | ([], [])      -> (* no more nodes to visit, return final distance *)
                     dist 
    | ([], xs)      -> (* done with all current neighbors, move to next rank *)
                     one_step (dist + 1) (xs, [])
    | (a::lt, xs) -> (* set a's distance to dist (unless already set); add unvisited neighbors to next rank *)
                     (match Hashtbl.find_opt graph.distance a with
                       | Some d -> one_step dist (lt, xs) (* node was already visited, skip *)
                       | None   -> Hashtbl.replace graph.distance a dist;
                                   let unseen = unexplored_neighbors a in
                                   one_step dist (lt ,(xs @ unseen))) in
  one_step 0 ([graph.origin], [])
  
  
(* debug *)
let string_of_tile = function
  | -1 -> "~" (* unexplored tile *)
  | 0  -> "#" (* wall *) 
  | 1  -> " " (* empty space *)
  | 2  -> "o" (* oxygen system *)
  | _  -> "?"

let string_of_point (i,j) = Printf.sprintf "(%d,%d)" i j

let string_of_neighbors neighbors =
  neighbors |> List.map string_of_point |> String.concat "; "

let print_hashtbl print_func h =
  let print_binding k v =
    Printf.printf "%s: %s\n" (string_of_point k) (print_func v) in
  Hashtbl.iter print_binding h
  
let g = "day15-maze.dat" 
  |> load_input
  |> build_graph
  |> compute_distances
  |> print_int
(*|> Utils.print_canvas string_of_tile *)

(*
let () = print_endline (string_of_point g.origin)
let (x, y) = g.origin
let () = print_endline (string_of_neighbors (Hashtbl.find g.neighbors (1,1))) *)