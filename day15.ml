#load "utils.cmo";;
#load "intercom.cmo";;

let program = ref ("day15.dat"
  |> Utils.read_program
  |> Intercom.init_coroutine)


let string_of_tile = function
  | -1 -> "~" (* unexplored tile *)
  | 0  -> "#" (* wall *) 
  | 1  -> " " (* empty space *)
  | 2  -> "o" (* oxygen system *)
  | 3  -> "D" (* droid *)
  | _  -> failwith "unknown tile type"

let h = 20

let screen = Array.make_matrix (2*h + 1) (2*h + 1) (-1)

let movement_of_key x y  = function
  | 'w' -> (3, x-1, y) (* go left/west *)
  | 'c' -> (4, x+1, y) (* go right/east *)
  | 's' -> (1, x, y+1) (* go up/north *)
  | 'x' -> (2, x, y-1) (* go down/south *)
  | _  -> failwith "unknown movement"

let reverse_move x y = function
  | 1 -> x, y-1
  | 2 -> x, y+1
  | 3 -> x+1, y
  | 4 -> x-1, y
  | _ -> failwith "unknown direction"

let draw_canvas x y =
  let old_val = screen.(x + h).(y + h) in
  screen.(x + h).(y + h) <- 3; 
  let _ = Utils.print_canvas string_of_tile screen in
  screen.(x + h).(y + h) <- old_val


let rec one_step x y prev_move =
  program := Intercom.execute !program;
  match !program.state with
    | Halted -> () (* shouldn't happen *)
    | WaitingForInput -> (match Utils.get1char() with
                        | 'q' | 'Q' -> () (* exit loop *)
                        | k -> let m, xx, yy = movement_of_key x y k in
                               program := Intercom.read_from_coroutine !program m;
                               one_step xx yy m)
    | ReturningOutput -> let tile = List.hd !program.output in
                         screen.(x + h).(y + h) <- tile;
                         let xx, yy = (if tile=0 then reverse_move x y prev_move  
                                                 else x, y) in
                         draw_canvas xx yy;
                         one_step xx yy 0

let _ = draw_canvas 0 0
let () = one_step 0 0 0
