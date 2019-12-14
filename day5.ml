type operation = Halt | Input | Output | Add | Mul | JumpTrue | JumpFalse | LessThan | Equals
type mode = Position | Immediate
type instruction = {opcode:operation; c:mode; b:mode; a:mode}

let string_of_operation = function
  | Halt -> "halt"
  | Input -> "input"
  | Output -> "output"
  | Add -> "add"
  | Mul -> "mul"
  | JumpTrue -> "jump if true"
  | JumpFalse -> "jump if false"
  | LessThan -> "less than"
  | Equals -> "equals"

let string_of_mode = function
  | Position -> "0"
  | Immediate -> "1"

let instruction_of_int n =
  let match_mode = function
    | 0 -> Position
    | 1 -> Immediate 
    | n -> failwith ("Unknown mode " ^ (string_of_int n)) in
  let operation = match (n mod 100) with
    | 1 -> Add
    | 2 -> Mul
    | 3 -> Input
    | 4 -> Output
    | 5 -> JumpTrue
    | 6 -> JumpFalse
    | 7 -> LessThan
    | 8 -> Equals
    | 99 -> Halt
    | _ -> failwith ("Unknown opcode " ^ (string_of_int n)) in
  let c_mode = match_mode ((n / 100) mod 10) in
  let b_mode = match_mode ((n / 1000) mod 10) in
  let a_mode = match_mode ((n / 10000) mod 10) in
  {opcode=operation; c=c_mode; b=b_mode; a=a_mode}

let string_of_instruction op =
  let operation_string = string_of_operation op.opcode in
  Printf.sprintf "%s %s%s%s" operation_string (string_of_mode op.c) (string_of_mode op.b) (string_of_mode op.a)

let binary_operation op a b =
  match op with
    | Add -> a + b
    | Mul -> a * b
    | LessThan -> if a < b then 1 else 0
    | Equals -> if a = b then 1 else 0
    | _ -> failwith ("Unknown binary operation")

(* return the referenced value based on the addressing mode *)
let operand program index = function
  | Immediate -> program.(index)
  | Position  -> program.(program.(index))

(* write the result of an instruction to the stack *)
let write program value index = function
  | Immediate -> let () = program.(index) <- value in index
  | Position  -> let () = program.(program.(index)) <- value in program.(index)

(* should we jump ? *)
let jump = function
  | JumpTrue,  0 -> false
  | JumpFalse, 0 -> true
  | JumpTrue,  _ -> true
  | JumpFalse, _ -> false
  | j,         _ -> failwith ("Unknown jump instruction " ^ (string_of_operation j))

(* recursively move through the instruction list, updating it as we go *)
let rec execute program ip =
  let op = instruction_of_int program.(ip) in
  (* let () = Printf.printf "ip=%d; op = %s %d %d\n" ip (string_of_instruction op) program.(ip + 1) program.(ip + 2) in *)
  match op.opcode with
    | Halt     -> ()
    | Output   -> let op1 = operand program (ip + 1) op.c in
                  let () = print_endline (string_of_int op1) in
                  execute program (ip + 2)
    | Input    -> let input = read_int() in
                  let target = write program input (ip + 1) Position in
                  let () = Printf.printf "Wrote %d to position %d\n" input target in
                  execute program (ip + 2)
    | JumpTrue | JumpFalse as j
               -> let op1 = operand program (ip + 1) op.c in
                  let target = operand program (ip + 2) op.b in
                  if jump (j, op1) then
                    execute program target
                  else
                    execute program (ip + 3)
	| bin_op   -> let op1 = operand program (ip + 1) op.c in
                  let op2 = operand program (ip + 2) op.b in
	              let result = binary_operation bin_op op1 op2 in
                  let target = write program result (ip + 3) op.a in
                  (* let () = Printf.printf "%s %d %d = %d written in %d\n" (string_of_operation bin_op) op1 op2 result target in *)
                  execute program (ip + 4)

let initial_state = [|3; 225; 1; 225; 6; 6; 1100; 1; 238; 225; 104; 0; 1102; 16; 13; 225; 1001; 88; 68; 224; 101; -114; 224; 224; 4; 224; 1002; 223; 8; 223; 1001; 224; 2; 224; 1; 223; 224; 223; 1101; 8; 76; 224; 101; -84; 224; 224; 4; 224; 102; 8; 223; 223; 101; 1; 224; 224; 1; 224; 223; 223; 1101; 63; 58; 225; 1102; 14; 56; 224; 101; -784; 224; 224; 4; 224; 102; 8; 223; 223; 101; 4; 224; 224; 1; 223; 224; 223; 1101; 29; 46; 225; 102; 60; 187; 224; 101; -2340; 224; 224; 4; 224; 102; 8; 223; 223; 101; 3; 224; 224; 1; 224; 223; 223; 1102; 60; 53; 225; 1101; 50; 52; 225; 2; 14; 218; 224; 101; -975; 224; 224; 4; 224; 102; 8; 223; 223; 1001; 224; 3; 224; 1; 223; 224; 223; 1002; 213; 79; 224; 101; -2291; 224; 224; 4; 224; 102; 8; 223; 223; 1001; 224; 2; 224; 1; 223; 224; 223; 1; 114; 117; 224; 101; -103; 224; 224; 4; 224; 1002; 223; 8; 223; 101; 4; 224; 224; 1; 224; 223; 223; 1101; 39; 47; 225; 101; 71; 61; 224; 101; -134; 224; 224; 4; 224; 102; 8; 223; 223; 101; 2; 224; 224; 1; 224; 223; 223; 1102; 29; 13; 225; 1102; 88; 75; 225; 4; 223; 99; 0; 0; 0; 677; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1105; 0; 99999; 1105; 227; 247; 1105; 1; 99999; 1005; 227; 99999; 1005; 0; 256; 1105; 1; 99999; 1106; 227; 99999; 1106; 0; 265; 1105; 1; 99999; 1006; 0; 99999; 1006; 227; 274; 1105; 1; 99999; 1105; 1; 280; 1105; 1; 99999; 1; 225; 225; 225; 1101; 294; 0; 0; 105; 1; 0; 1105; 1; 99999; 1106; 0; 300; 1105; 1; 99999; 1; 225; 225; 225; 1101; 314; 0; 0; 106; 0; 0; 1105; 1; 99999; 1107; 677; 677; 224; 102; 2; 223; 223; 1006; 224; 329; 1001; 223; 1; 223; 108; 677; 677; 224; 1002; 223; 2; 223; 1005; 224; 344; 101; 1; 223; 223; 1008; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 359; 1001; 223; 1; 223; 1107; 226; 677; 224; 102; 2; 223; 223; 1006; 224; 374; 1001; 223; 1; 223; 8; 677; 226; 224; 102; 2; 223; 223; 1006; 224; 389; 101; 1; 223; 223; 8; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 404; 101; 1; 223; 223; 7; 677; 677; 224; 1002; 223; 2; 223; 1006; 224; 419; 101; 1; 223; 223; 7; 677; 226; 224; 1002; 223; 2; 223; 1005; 224; 434; 101; 1; 223; 223; 1108; 677; 226; 224; 1002; 223; 2; 223; 1006; 224; 449; 1001; 223; 1; 223; 108; 677; 226; 224; 1002; 223; 2; 223; 1006; 224; 464; 101; 1; 223; 223; 1108; 226; 677; 224; 1002; 223; 2; 223; 1006; 224; 479; 101; 1; 223; 223; 1007; 677; 677; 224; 1002; 223; 2; 223; 1006; 224; 494; 1001; 223; 1; 223; 107; 226; 226; 224; 102; 2; 223; 223; 1005; 224; 509; 1001; 223; 1; 223; 1008; 677; 226; 224; 102; 2; 223; 223; 1005; 224; 524; 1001; 223; 1; 223; 1007; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 539; 101; 1; 223; 223; 1108; 677; 677; 224; 102; 2; 223; 223; 1005; 224; 554; 1001; 223; 1; 223; 1008; 677; 677; 224; 1002; 223; 2; 223; 1006; 224; 569; 101; 1; 223; 223; 1107; 677; 226; 224; 1002; 223; 2; 223; 1006; 224; 584; 1001; 223; 1; 223; 7; 226; 677; 224; 102; 2; 223; 223; 1005; 224; 599; 101; 1; 223; 223; 108; 226; 226; 224; 1002; 223; 2; 223; 1005; 224; 614; 101; 1; 223; 223; 107; 226; 677; 224; 1002; 223; 2; 223; 1005; 224; 629; 1001; 223; 1; 223; 107; 677; 677; 224; 1002; 223; 2; 223; 1006; 224; 644; 101; 1; 223; 223; 1007; 677; 226; 224; 1002; 223; 2; 223; 1006; 224; 659; 101; 1; 223; 223; 8; 226; 677; 224; 102; 2; 223; 223; 1005; 224; 674; 1001; 223; 1; 223; 4; 223; 99; 226|]

let () = execute initial_state 0
