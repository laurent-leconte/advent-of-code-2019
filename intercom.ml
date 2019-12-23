type operation = Halt | Input | Output | Add | Mul | JumpTrue | JumpFalse | LessThan | Equals
type mode = Position | Immediate
type instruction = {opcode:operation; c:mode; b:mode; a:mode}
type program = {ram: int array; mutable ip: int; mutable input: int list; mutable output: int list}

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

let init_program (prog, inputs) =
  {ram = Array.copy prog;
   ip = 0;
   input = inputs;
   output = []}

(* either consume one item from the input list or get an int from std_in, if the list is empty *)
let read_input prog =
  match prog.input with
    | []    -> print_endline("Please enter a number");
               read_int()
    | x::xs -> prog.input <- xs;
               x

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
  | Immediate -> program.(index) <- value
  | Position  -> program.(program.(index)) <- value

(* should we jump ? *)
let jump = function
  | JumpTrue,  0 -> false
  | JumpFalse, 0 -> true
  | JumpTrue,  _ -> true
  | JumpFalse, _ -> false
  | j,         _ -> failwith ("Unknown jump instruction " ^ (string_of_operation j))

(* recursively move through the instruction list, updating it as we go *)
let rec execute prog =
  let ip = prog.ip in
  let op = instruction_of_int prog.ram.(ip) in
  match op.opcode with
    | Halt     -> prog
    | Output   -> let output_value = operand prog.ram (ip + 1) op.c in
                  print_endline (string_of_int output_value);
                  prog.output <- output_value::prog.output;
                  prog.ip <- ip + 2;
                  execute prog
    | Input    -> let input = read_input prog in
                  write prog.ram input (ip + 1) Position;
                  prog.ip <- ip + 2;
                    execute prog
    | JumpTrue | JumpFalse as j
               -> let op1 = operand prog.ram (ip + 1) op.c in
                  let target = operand prog.ram (ip + 2) op.b in
                  if jump (j, op1) then
                    prog.ip <- target
                  else
                    prog.ip <- ip + 3;
                  execute prog
	| bin_op   -> let op1 = operand prog.ram (ip + 1) op.c in
                  let op2 = operand prog.ram (ip + 2) op.b in
	              let result = binary_operation bin_op op1 op2 in
                  write prog.ram result (ip + 3) op.a;
                  prog.ip <- ip + 4;
                  execute prog
