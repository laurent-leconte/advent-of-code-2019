type point = {x: int; y: int}

module Direction =
  struct
    type t = point
    
    let is_zero p = (p.x = 0) && (p.y = 0)
    
    let angle p =
      let n = sqrt (float_of_int (p.x*p.x + p.y*p.y)) in
      let theta = acos ((float_of_int p.y) /. n) in
      let pi = acos (-1.0) in
      if p.x >= 0 then theta else 2.*.pi -. theta
    
    let compare p q =
    (* comparison rules: 
    * 0 = 0
    * 0 < _
    * a = b iff same direction and orientation 
    * otherwise angle from y-axis is used for comparison *)
      if p = q          then 0    (* handles equality, including 0 = 0 *)
      else if is_zero p then (-1) (* 0 is smaller than everything *)
      else if is_zero q then 1
      else (* p, q are non-zero and different *)
      begin
        let x0, y0 = p.x, p.y in
        let x1, y1 = q.x, q.y in
        if x0*y1 - y0*x1 = 0 && x0*x1 >= 0 then 0 (* use integer precision for equality *)
        else compare (angle p) (angle q)
      end
  end

module LinesOfSight = Set.Make(Direction)

let string_of_point p = Printf.sprintf "(%d, %d)" p.x p.y

let print_directions =
  let print_point p = print_endline (string_of_point p) in
  LinesOfSight.iter print_point

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)

let delta p1 p2 = {x = p2.x - p1.x; y = p2.y - p1.y}

(* count how many other points are visible from `here` *)
let count_visible points here =
  let direction_from_here = delta here in
  (points 
  |> List.map direction_from_here (* build all directions between `here` and the list of points *)
  |> List.fold_left (fun s elt -> LinesOfSight.add elt s) LinesOfSight.empty (* put them into a set to keep uniques *)
  |> LinesOfSight.cardinal) - 1 (* return the size of the set minus 1 to account for `here` *)

(* copy a list *)
let rec copy = function
  | []    -> []
  | a::tl -> a::(copy tl)
  
(* find the max of an int list *)
let max_of_list l =
  let rec loop max_so_far = function
    | [] -> max_so_far
    | a::tl -> loop (max a max_so_far) tl in
  loop min_int l

let count_all_visibles coords =
  let copy_coords = copy coords in
  List.map (count_visible copy_coords) coords

let read_lines name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in 
  loop []

let points_from_line n s =
  let result = ref [] in
  let coord_from_char idx = function
    | '#' -> result := !result @ [{x=idx; y=n}]
    | _ -> () in
  String.iteri coord_from_char s;
  !result

let points_from_input lines =
  lines
  |> List.mapi points_from_line
  |> List.flatten

let rec test_compare = function
 [] | [_] -> ()
 |a::(b::_ as tl) -> Printf.printf "%s - %s : %d\n" (string_of_point a) (string_of_point b) (Direction.compare a b);
                     test_compare tl
  
let _ = "day10.dat" 
  |> read_lines
  |> points_from_input
  |> count_all_visibles
  |> max_of_list
  |> print_int