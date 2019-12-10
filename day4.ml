let rec digits = function
  | 0 -> []
  | n -> (n mod 10)::digits (n / 10);;
  
  
let meets_part1_criteria number =
  let rec meets_criteria_iter adjacent = function
    | [] | [_] -> adjacent
    | a::(b::_ as tl) -> if a = b then
                           (* we found adjacent similar digits: set adjacent to true*)
                           meets_criteria_iter true tl
                         else if a > b then
                           (* increasing digits (keep in mind the list is reversed), carry on *)
                           meets_criteria_iter adjacent tl
                         else
                           (* decreasing digits; return false *)
                         false in
  meets_criteria_iter false (digits number);;


let meets_part2_criteria number =
  let rec growing = function
    | [] | [_] -> true
	| a::(b::_ as tl) -> (a >= b) && growing tl in
  let rec adjacent previous = function
    (* return true once two (and only two) adjacent values are equal *)
    | [] | [_]           -> false
    | a::b::[]           -> (a = b) && (a  <> previous) (* last 2 digits are equal *)
    | a::(b::c::_ as tl) -> (a <> previous && a = b && b <> c) (* a and b are equal and different from their neighbors -> true *)
	                        || (adjacent a tl) in
  let digit_list = digits number in
  growing @@ digit_list && (adjacent ~-1 digit_list);;

let brute_force meets_criteria start stop =
  let rec brute_iter acc current =
    if current = stop then
      acc
    else
      let increase = meets_criteria current in
      brute_iter (if increase then (acc + 1) else acc) (current + 1) in
  brute_iter 0 start;;

print_endline @@ string_of_int (brute_force meets_part1_criteria 265275 781584);;


print_endline @@ string_of_int (brute_force meets_part2_criteria 265275 781584);;

(*
let tests = [11111; 1223456789; 223450; 123568] in
print_endline (String.concat " " (List.map (fun x -> string_of_bool (meets_criteria x)) tests));; *)