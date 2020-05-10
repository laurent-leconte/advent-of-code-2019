#load "intercom.cmo";;
#load "utils.cmo";;

type color = Black | White

let color_of_int = function
  | 0 -> Black
  | 1 -> White
  | _ -> failwith "unknown color code"

let int_of_color = function
  | Black -> 0
  | White -> 1

type action = {
  x: int;
  y: int;
  applied_color: color
}

let string_of_action act = Printf.sprintf "(%d, %d): %d" act.x act.y (int_of_color act.applied_color)

type robot = {
  mutable x: int;
  mutable y: int;
  mutable dir: int; (* 0: up; 1: right; 2: down; 3: left *)
  mutable prog: Intercom.program;
  mutable action_log: action list
}

let init_robot instructions = {
  x = 0;
  y = 0;
  dir = 0;
  prog = Intercom.init_coroutine instructions;
  action_log = []}
  
(* go through the action log to figure out the current color of a tile *)
let current_color x y log =
  let rec loop acc  (log:action list) =
    match log with
      | []     -> acc
      | hd::tl -> if hd.x = x && hd.y = y then
                    loop hd.applied_color tl   
                  else
                    loop acc tl in
  let initial_color = match (x, y) with
    | 0,0 -> White
    | _   -> Black in
  loop initial_color log

let new_direction old_dir = function
  | 0 -> (old_dir + 3) mod 4 (* turn left *)
  | 1 -> (old_dir + 1) mod 4 (* turn right *)
  | _ -> failwith "unknown turn instruction"


let move_robot robot =
  (match robot.dir with
    | 0 -> robot.y <- robot.y + 1
    | 1 -> robot.x <- robot.x + 1
    | 2 -> robot.y <- robot.y - 1
    | 3 -> robot.x <- robot.x - 1);
  robot

let rec run_robot robot =
  robot.prog <- Intercom.execute robot.prog;
  match robot.prog.state with
    | Halted          -> robot
    | WaitingForInput -> let color = int_of_color (current_color robot.x robot.y robot.action_log) in
                         robot.prog <- Intercom.read_from_coroutine robot.prog color;
                         run_robot robot
    | ReturningOutput -> let new_color = List.hd robot.prog.output in
                         let action = {x = robot.x; y = robot.y; applied_color = color_of_int new_color} in
                         robot.action_log <- robot.action_log @ [action]; (* log new painted tile *)
                         robot.prog <- Intercom.execute robot.prog; (* execute robot till next output *)
                         let turn = List.hd robot.prog.output in
                         robot.dir <- new_direction robot.dir turn;
                         run_robot (move_robot robot)
    | _               -> failwith "unknown state"

              
let instructions = [|3;8;1005;8;304;1106;0;11;0;0;0;104;1;104;0;3;8;102;-1;8;10;101;1;10;10;4;10;1008;8;1;10;4;10;1002;8;1;29;2;103;1;10;1;106;18;10;3;8;102;-1;8;10;1001;10;1;10;4;10;1008;8;1;10;4;10;102;1;8;59;2;102;3;10;2;1101;12;10;3;8;102;-1;8;10;1001;10;1;10;4;10;108;0;8;10;4;10;101;0;8;88;3;8;102;-1;8;10;1001;10;1;10;4;10;108;1;8;10;4;10;101;0;8;110;2;108;9;10;1006;0;56;3;8;102;-1;8;10;1001;10;1;10;4;10;108;0;8;10;4;10;101;0;8;139;1;108;20;10;3;8;102;-1;8;10;101;1;10;10;4;10;108;0;8;10;4;10;102;1;8;165;1;104;9;10;3;8;102;-1;8;10;101;1;10;10;4;10;1008;8;0;10;4;10;1001;8;0;192;2;9;14;10;2;1103;5;10;1;1108;5;10;3;8;1002;8;-1;10;101;1;10;10;4;10;1008;8;1;10;4;10;102;1;8;226;1006;0;73;1006;0;20;1;1106;11;10;1;1105;7;10;3;8;102;-1;8;10;1001;10;1;10;4;10;108;0;8;10;4;10;1001;8;0;261;3;8;102;-1;8;10;101;1;10;10;4;10;108;1;8;10;4;10;1002;8;1;283;101;1;9;9;1007;9;1052;10;1005;10;15;99;109;626;104;0;104;1;21101;48062899092;0;1;21101;0;321;0;1105;1;425;21101;936995300108;0;1;21101;0;332;0;1106;0;425;3;10;104;0;104;1;3;10;104;0;104;0;3;10;104;0;104;1;3;10;104;0;104;1;3;10;104;0;104;0;3;10;104;0;104;1;21102;209382902951;1;1;21101;379;0;0;1106;0;425;21102;179544747200;1;1;21102;390;1;0;1106;0;425;3;10;104;0;104;0;3;10;104;0;104;0;21102;1;709488292628;1;21102;1;413;0;1106;0;425;21101;0;983929868648;1;21101;424;0;0;1105;1;425;99;109;2;22101;0;-1;1;21102;40;1;2;21102;456;1;3;21101;446;0;0;1106;0;489;109;-2;2106;0;0;0;1;0;0;1;109;2;3;10;204;-1;1001;451;452;467;4;0;1001;451;1;451;108;4;451;10;1006;10;483;1102;0;1;451;109;-2;2105;1;0;0;109;4;1201;-1;0;488;1207;-3;0;10;1006;10;506;21102;1;0;-3;21202;-3;1;1;21201;-2;0;2;21101;0;1;3;21101;525;0;0;1105;1;530;109;-4;2105;1;0;109;5;1207;-3;1;10;1006;10;553;2207;-4;-2;10;1006;10;553;21202;-4;1;-4;1105;1;621;21201;-4;0;1;21201;-3;-1;2;21202;-2;2;3;21102;1;572;0;1106;0;530;21201;1;0;-4;21101;0;1;-1;2207;-4;-2;10;1006;10;591;21102;0;1;-1;22202;-2;-1;-2;2107;0;-3;10;1006;10;613;22101;0;-1;1;21101;0;613;0;106;0;488;21202;-2;-1;-2;22201;-4;-2;-4;109;-5;2106;0;0|]

let robot = instructions |> init_robot |> run_robot

(* int,int set to count unique coordinates *)
module CoordSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = int*int
  end )

let bounding_box coordinates =
  let union (xmin, xmax, ymin, ymax) (x,y) = (min xmin x, max xmax x, min ymin y, max ymax y) in
  let (x0, y0) = List.hd coordinates in
  List.fold_left union (x0, x0, y0, y0) coordinates

let paint (xmin, xmax, ymin, ymax) action_log =
  let canvas = Array.init (xmax - xmin + 1) (fun i -> Array.make (ymax - ymin + 1) Black) in
  canvas.(0 - xmin).(0 - ymin) <- White; (* init first square to white *)
  let paint_one (a:action) =
    canvas.(a.x - xmin).(a.y - ymin) <- a.applied_color; in
  let _ = List.map paint_one action_log in
  canvas

let string_of_color = function
  | Black -> " "
  | White -> "#"

(* let print_canvas canvas width height =
  for j = 0 to height do
    for i = 0 to width do
      print_string (string_of_color canvas.(i).(j))
    done
  done *)

let xmin, xmax, ymin, ymax = robot.action_log 
  |> List.map (fun (a:action) -> (a.x, a.y)) 
  |> bounding_box
  
let canvas = paint (xmin, xmax, ymin, ymax) robot.action_log

let  _ = Utils.print_canvas string_of_color canvas




