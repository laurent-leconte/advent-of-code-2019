#load "intercom.cmo";;

(* Pop nth element of a list, return element and remainder of the list *)
let pop_nth l n =
  let rec loop previous rest = function
    | 0 -> (List.hd rest, previous @ (List.tl rest))
    | i -> loop (previous @ [List.hd rest]) (List.tl rest) (i - 1) in
  loop [] l n

let rec range a b = 
  if a=b then [] else a::(range (a+1) b)

(* Decompose n in the factorial number system. Returns a list of length k
   See https://en.wikipedia.org/wiki/Factorial_number_system *)
let factoradic k n=
  let rec loop acc i n = function
    | 0 -> acc
    | k -> let rem = n mod i in
           loop (rem::acc) (i+1) (n/i) (k-1) in
  loop [] 1 n k

(* Generate a permutation of [|0..k-1|] from an int between 0 and n!-1.
   See https://en.wikipedia.org/wiki/Factorial_number_system 
   for an explanation of the algorithm *)
let perm_of_int k n =
  let ints = range 0 k in
  let rec build_perm acc remaining = function
    | [] -> acc @ remaining
    | x::xs -> let next, new_remaining = pop_nth remaining x in
               build_perm (next::acc) new_remaining xs in  
  build_perm [] ints (factoradic k n)


let generate_all_perms k =
  let rec factorial n = if n=1 then 1 else n*(factorial (n-1)) in
  let rec loop acc = function
    | -1 -> acc
    | n -> loop ((perm_of_int k n)::acc) (n-1) in
  loop [] ((factorial k) - 1)

let string_of_list l = String.concat " " (List.map string_of_int l)

let thruster_power program sequence =
  let rec next_amp output = function
    | [] -> output
    | x::xs -> let final_state = (program, [x; output])
                                 |> Intercom.init_program 
                                 |> Intercom.execute in
               next_amp (List.hd final_state.output) xs in
  next_amp 0 sequence

let input = [|3;8;1001;8;10;8;105;1;0;0;21;34;59;68;89;102;183;264;345;426;99999;3;9;102;5;9;9;1001;9;5;9;4;9;99;3;9;101;3;9;9;1002;9;5;9;101;5;9;9;1002;9;3;9;1001;9;5;9;4;9;99;3;9;101;5;9;9;4;9;99;3;9;102;4;9;9;101;3;9;9;102;5;9;9;101;4;9;9;4;9;99;3;9;1002;9;5;9;1001;9;2;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;99;3;9;1001;9;1;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;99;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;99;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;3;9;1002;9;2;9;4;9;99|]

let _ = generate_all_perms 5 
  |> List.map (thruster_power input)
  |> List.sort compare 
  |> string_of_list
  |> print_endline