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
    * otherwise clockwise angle from y-axis is used for comparison *)
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

(* helper functions *)
let string_of_point p = Printf.sprintf "(%d, %d)" p.x p.y

let string_of_point_list l =
  "[" ^ (l |> List.map string_of_point |> String.concat ";") ^ "]"

let print_directions =
  let print_point p = print_endline (string_of_point p) in
  LinesOfSight.iter print_point

(* directions are stored with traditional coordinates (y points up)
   whereas Y coordinate for meteors point down *)
let delta p1 p2 = {x = p2.x - p1.x; y = p1.y - p2.y}

(* count how many other points are visible from `here` *)
let count_visible points here =
  let direction_from_here = delta here in
  let count = (points 
  |> List.map direction_from_here (* build all directions between `here` and the list of points *)
  |> List.fold_left (fun s elt -> LinesOfSight.add elt s) LinesOfSight.empty (* put them into a set to keep uniques *)
  |> LinesOfSight.cardinal) - 1 in (* count is size of the set minus 1 to account for `here` *)
  (count, here)

(* copy a list *)
let rec copy = function
  | []    -> []
  | a::tl -> a::(copy tl)
  
(* find the max of an (int, point) list *)
let max_of_list l =
  let max a b = if fst a > fst b then a else b in
  List.fold_left max (List.hd l) (List.tl l)

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

(* part 1 *)
let max, spot = "day10.dat" 
  |> read_lines
  |> points_from_input
  |> count_all_visibles
  |> max_of_list

let () = Printf.printf "Part 1: %d (%d, %d)\n" max spot.x spot.y
  
  
(* insert a point in a list, keeping it sorted by increasing distance *)
let rec insert_by_distance p = function
  | []    -> [p]
  | a::tl -> let px, py = p.x, p.y in
             let ax, ay = a.x, a.y in
             if px*px + py*py < ax*ax + ay*ay then
               p::a::tl
             else
               a::(insert_by_distance p tl)

module AlignedPoints = Map.Make(Direction)

let add_point m p =
  match AlignedPoints.find_opt p m with
    | None   -> AlignedPoints.add p [p] m
    | Some l -> AlignedPoints.add p (insert_by_distance p l) m

let build_alignment_from here points =
  let direction_from_here = delta here in
  points 
  |> List.map direction_from_here (* build all directions between `here` and the list of points *)
  |> List.fold_left add_point AlignedPoints.empty (* add them to a direction -> points map *)

(* equivalent to List.map List.tl but ignores empty lists and empty tails *)
let rec map_tl = function
  | []           -> []
  | []::xs
  | (_::[])::xs  -> map_tl xs
  | (_::xs)::xss -> xs :: map_tl xss

(* transpose and flatten a list of list. The lists may be of different lenghths *)
let rec flat_transpose = function
  | [] -> []
  | []::xs -> flat_transpose xs  
  | x      -> let first_items = List.map List.hd x in
              let rest =  map_tl x in
              (*print_endline ("First items: " ^ (string_of_point_list first_items));
              print_endline ("Rest: " ^ (String.concat " " (List.map string_of_point_list rest))); *)
              first_items @ flat_transpose (rest)


(* part 2 *)
let alignments = "day10.dat"
  |> read_lines
  |> points_from_input
  |> build_alignment_from spot
  |> AlignedPoints.remove {x=0; y=0}

let flattened = AlignedPoints.fold (fun k v a -> a@[v]) alignments []
  |> flat_transpose

let target_delta = List.nth flattened 199
let target = {x = spot.x + target_delta.x; y = spot.y - target_delta.y}
let () = Printf.printf "Part 2: %d\n" (target.x * 100 + target.y)
