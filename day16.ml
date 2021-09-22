let split_string s =
  let int_of_char c = int_of_string @@ Char.escaped c in
  s |> String.to_seq (* transform into a char seq *)
    |> Seq.map (int_of_char)
    |> Array.of_seq
    
let make_vect ar =
  let n = Array.length ar in
  let matrix = Array.make_matrix n 1 0 in
  for i = 0 to (n - 1) do
    matrix.(i).(0) <- ar.(i)
  done;
  matrix

let generate_matrix n =
  let pattern = [|0; 1; 0; (-1)|] in
  let matrix = Array.make_matrix n n 0 in
  for l = 1 to n do
    for j = 1 to n do
      (* line l: 0...0 (l-1 times) 1...1 (l times) 0...0 (l times) (-1...-1) (l times) *)
      matrix.(l - 1).(j - 1) <- pattern.((j / l) mod 4)
    done
  done;
  matrix
  
let id n =
  let matrix = Array.make_matrix n n 0 in
  for i = 0 to (n - 1) do matrix.(i).(i) <- 1 done;
  matrix
  
(* lifted from https://gist.github.com/xysun/5950145 *)
let mult m0 m1 = 
  let x0 = Array.length m0 and y0 = Array.length m0.(0) and
    x1 = Array.length m1 and y1 = Array.length m1.(0) in
  if y0 <> x1 then failwith "incompatible dimensions!"
  else
    let res_matrix = Array.make_matrix x0 y1 0 in
    for i = 0 to x0 - 1 do
      for j = 0 to y1 - 1 do
        for k = 0 to y0 - 1 do
          res_matrix.(i).(j) <- (res_matrix.(i).(j) + m0.(i).(k) * m1.(k).(j)) mod 10
        done
      done
    done;
    res_matrix
  
  
let abs_sum_prod ar1 ar2 =
  abs (Array.fold_left (+) 0 (Array.map2 ( * ) ar1 ar2)) mod 10

(* compute mat**k mod 10 *)
let mod_exp mat k =
  let n = Array.length mat in
  let rec aux acc res = function
    | 0 -> res
    | n -> let new_res = if (n mod 2) = 0 then res else mult acc res in
           let new_acc = mult acc acc in
           aux new_acc new_res (n / 2) in
  aux mat (id n) k

let part1 steps input_string =
  let input = split_string input_string in
  let n = Array.length input in
  let m = generate_matrix n in
  let rec loop acc = function
    | 0 -> acc
    | k -> let new_acc = Array.make n 0 in
           for i = 0 to (n-1) do
             new_acc.(i) <- abs_sum_prod m.(i) acc;
           done;
           loop new_acc (k-1) in
  loop input steps

(* debug and print functions *)  
let print_matrix m =
  let padded_int i = Printf.sprintf "%4d" i in
  let lists = Array.to_list (Array.map Array.to_list m) in
  let print_line l = 
    l |> List.map (int_to_string) |> String.concat "" |> print_endline in
  List.map print_line (lists)

let print_array ar =
  let padded_int i = Printf.sprintf "%2d" i in
  ar |> Array.to_list |> List.map padded_int |> String.concat "" |> print_endline

let _ = "59762574510031092870627555978901048140761858379740610694074091049186715780458779281173757827279664853239780029412670100985236587608814782710381775353184676765362101185238452198186925468994552552398595814359309282056989047272499461615390684945613327635342384979527937787179298170470398889777345335944061895986118963644324482739546009761011573063020753536341827987918039441655270976866933694280743472164322345885084587955296513566305016045735446107160972309130456411097870723829697443958231034895802811058095753929607703384342912790841710546106752652278155618050157828313372657706962936077252259769356590996872429312866133190813912508915591107648889331"
  |> part1 100
  |> print_array