let operand_of_int = function
  | 1 -> (+)
  | 2 -> ( * )
  | x -> failwith ("Unknown opcode " ^ (string_of_int x));;

(* recursively move through the instruction list, updating it as we go *)
let rec execute program ip = 
  let op = program.(ip) in
  match op with
    | 99    -> program
	| x     -> let op1 = program.(program.(ip + 1)) in
               let op2 = program.(program.(ip + 2)) in
	           let target = program.(ip + 3) in
			   let target_value = operand_of_int op op1 op2 in
			   (* let _ = Printf.printf "%d %d %d = %d written in %d\n" op op1 op2 target_value target in *)
               let () = program.(target) <- target_value in
               execute program (ip + 4);;

let initial_state = [| 1; 0; 0; 3; 1; 1; 2; 3; 1; 3; 4; 3; 1; 5; 0; 3; 2; 1; 10; 19; 1; 19; 5; 23; 2; 23; 6; 27; 1; 27; 5; 31; 2; 6; 31; 35; 1; 5; 35; 39; 2; 39; 9; 43; 1; 43; 5; 47; 1; 10; 47; 51; 1; 51; 6; 55; 1; 55; 10; 59; 1; 59; 6; 63; 2; 13; 63; 67; 1; 9; 67; 71; 2; 6; 71; 75; 1; 5; 75; 79; 1; 9; 79; 83; 2; 6; 83; 87; 1; 5; 87; 91; 2; 6; 91; 95; 2; 95; 9; 99; 1; 99; 6; 103; 1; 103; 13; 107; 2; 13; 107; 111; 2; 111; 10; 115; 1; 115; 6; 119; 1; 6; 119; 123; 2; 6; 123; 127; 1; 127; 5; 131; 2; 131; 6; 135; 1; 135; 2; 139; 1; 139; 9; 0; 99; 2; 14; 0; 0 |];;

let test_inputs noun verb =
  let state = Array.copy initial_state in
  let () = state.(1) <- noun in
  let () = state.(2) <- verb in
  let final_state = execute state 0 in
  final_state.(0);;
  
print_endline ("Part 1 :" ^ string_of_int (test_inputs 12 2));;

let test_all_inputs target max1 max2 =
  let rec test_iter m n =
    let result = test_inputs m n in
    if result = target then
      100*m + n
    else
	  (* for some reason | max1, max2 is always matched but | 0, 0 is ok so I had to make the loop count backward *)
	  match m, n with
	    | 0, 0 -> -1 (* we reached the end of the range and didn't find our expected result *)
		| m, 0 -> test_iter (m - 1) max2
		| m, n -> test_iter m (n -1) in
  test_iter max1 max2;;
  
print_endline ("Part 2 :" ^ string_of_int (test_all_inputs 19690720 99 99));;
      	
  