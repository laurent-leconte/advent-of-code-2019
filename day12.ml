type coordinates = {x: int; y: int; z: int}
type moon = {
  mutable v: coordinates;
  mutable p: coordinates;
}

let coords_of_tuple (x, y, z) = {x = x; y = y; z = z}
  
let string_of_coord c = Printf.sprintf "(%d, %d, %d)" c.x c.y c.z

let string_of_moon m = Printf.sprintf "p: %s; v: %s" (string_of_coord m.p) (string_of_coord m.v)

let make_moon xyz =
  {
    p = coords_of_tuple xyz;
    v = coords_of_tuple (0, 0, 0)
  }

let potential_energy moon =
  let abs_sum c = abs c.x + abs c.y + abs c.z in
  abs_sum moon.v * abs_sum moon.p

let add c1 c2 = {
    x = c1.x + c2.x;
    y = c1.y + c2.y;
    z = c1.z + c2.z
  }
  
let scalar l c = {
    x = l * c.x;
    y = l * c.y;
    z = l * c.z
  }
  
let sub c1 c2 = add c1 (scalar (-1) c2)

let delta_p moon1 moon2 = {
    x = compare moon1.p.x moon2.p.x;
    y = compare moon1.p.y moon2.p.y;
    z = compare moon1.p.z moon2.p.z
  }

let apply_velocity moon =
  let new_p = {
    x = moon.p.x + moon.v.x;
    y = moon.p.y + moon.v.y;
    z = moon.p.z + moon.v.z
  } in
  moon.p <- new_p

let apply_gravity moon1 moon2 =
  let dp = delta_p moon2 moon1 in
  moon1.v <- add moon1.v dp;
  moon2.v <- sub moon2.v dp

let pairs n = 
  let rec range k = if (k >= n) then [] else k::range (k+1) in
  let rec combine a = function
    | []    -> []
    | b::tl -> (a, b)::(combine a tl) in
  let rec loop = function  
    | []    -> []
    | a::tl -> (combine a tl) @ loop tl in
  loop (range 0)

(* apply gravity to all pairs of moons in the system, then apply velocity to all moons *)
let rec one_step system = function
  | []        -> let _ = Array.map apply_velocity system in
                 system
  | (i,j)::tl -> apply_gravity system.(i) system.(j);
                 one_step system tl

let n_steps steps system =
  let all_pairs = pairs (Array.length system) in
  let rec loop = function
    | 0 -> system
    | n -> let _ = one_step system all_pairs in
           loop (n-1) in
  loop steps

let total_energy_after_n_steps coords n =
  coords
    |> Array.map make_moon (* build system *) 
    |> n_steps n (* simulate n steps *)
    |> Array.map potential_energy (* potential energy of each moon *)
    |> Array.fold_left (+) 0 (* sum all potential energies *)


let example1 = total_energy_after_n_steps [|(-1, 0, 2);(2, -10, -7);(4, -8, 8);(3, 5, - 1)|] 10

let example2 = total_energy_after_n_steps [|(-8, -10, 0);(5, 5, 10);(2, -7, 3);(9, -8, -3)|] 100

let part1 = total_energy_after_n_steps [|(14, 4, 5);(12, 10, 8);(1, 7, -10);(16, -5, 3)|] 1000

let () = print_int part1
