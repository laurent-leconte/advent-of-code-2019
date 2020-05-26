#load "utils.cmo";;
#load "str.cma";;


(* helper function to split on a given string *)
let split_on_string s = Str.split (Str.regexp_string s)

(* parse a formula: 
 - build a hash table with ingredient as keys and quantities as values. "output" is used for the output key 
 - return the pair (output name, hash table) *) 
let parse_line l =
  (* split input and output parts of the formula *)
  let input::output::_ = split_on_string " => " l in
  (* split raw input ingredients *)
  let raw_inputs = split_on_string ", " input in
  let tuple_from_raw str =
    let quantity::ingredient::_ = split_on_string " " str in
    (ingredient, float_of_string quantity) in
  (* create formula hash table, add the output quantity *)
  let formula = Hashtbl.create (List.length raw_inputs + 1) in
  let output_name, output_qt = tuple_from_raw output in
  Hashtbl.add formula "output" output_qt;
  let add_tuple t =
    Hashtbl.add formula (fst t) (snd t); in
  (* add raw inputs to the hash table *)
  let _ = raw_inputs
    |> List.map tuple_from_raw
    |> List.map add_tuple in
  (output_name, formula)

let formula_output f = fst (List.hd f)

(* debug functions *)
let list_of_hash h =
  let f k v acc = (k,v) :: acc in
  Hashtbl.fold f h []
let string_of_ingredient a = (string_of_float @@ snd a) ^ " " ^ (fst a)
let string_of_formula f = f
 |> list_of_hash
 |> List.map string_of_ingredient
 |> String.concat "; "
let string_of_list l = "[" ^ (String.concat "; " l) ^ "]" 

(* ingest a list of formulas and add them to a cookbook hashtable *)
let create_cookbook formulas =
  let cookbook = Hashtbl.create (List.length formulas) in
  let add_to_cookbook formula =
    Hashtbl.add cookbook (fst formula) (snd formula) in
  let _ = formulas
    |> List.map parse_line
    |> List.map add_to_cookbook in
  cookbook

(* from a cookbook, build its dependency map
This is a Hashtbl where the keys are the different ingredients in the cookbook
(i.e. same keys as the input Hashtbl) and the values are pairs (in_degree, list of out_nodes) *)
let dependency_map h =
  let map = Hashtbl.create (Hashtbl.length h) in
  let seq_to_list = Seq.fold_left (fun tl hd -> hd::tl) [] in
  (* build a filtered list of the hashtable keys *)
  let clean_inputs v  = (v 
    |> Hashtbl.to_seq_keys 
    |> Seq.filter ((<>) "output") 
    |> seq_to_list) in
  (* handle one binding from the input cookbook *)
  let make_one k v =
    (* increase the in_degree of a key, creating the binding if it doesn't exist *)
    let inc_in_degree k =
      let new_val = match Hashtbl.find_opt map k with
        | None -> (1, [])
        | Some (a, l) -> (a + 1, l) in
      Hashtbl.replace map k new_val in
    (* get ingredients and increase their in_degree *)
    let dependents = clean_inputs v in
    let _ = List.map inc_in_degree dependents in
  (* get existing binding for the key and update the list of ingredients *)    
  let new_val = match Hashtbl.find_opt map k with
      | None -> (0, dependents)
      | Some (a, []) -> (a, dependents) in
    Hashtbl.replace map k new_val in
  let () = Hashtbl.iter make_one h in
  map

let topological_sort map =
  (* "remove" (actually do nothing) a node and decrease its out_nodes' in_degrees.
     returns a list of new nodes with in_degree 0 *)
  let remove_and_decrease k =
    let (_, children) = Hashtbl.find map k in
    (* decreate the in_degree of a node and return its key if the nex in_degree is 0 *)
    let decrease_in_degree c = 
      let (d, l) = Hashtbl.find map c in
      Hashtbl.replace map c ((d-1), l);
      match d with
        | 1 -> Some c
        | _ -> None in
    List.filter_map decrease_in_degree children in
  (* walk through the graph, starting with "FUEL" and adding all 0-in_degree nodes sequentially *)
  let rec pop_n_sort acc = function
    | []    -> acc
    | a::tl -> let next = remove_and_decrease a in
               pop_n_sort (a::acc) (tl @ next) in
  List.rev (pop_n_sort [] ["FUEL"])


let expand_fuel_formula cookbook =
  let fuel_formula = Hashtbl.find cookbook "FUEL" in
  let rec expand = function
    | [] 
    | ["ORE"] -> fuel_formula 
    | a::tl   -> (* replace a with its ingredients in "FUEL" formula *)
                 (* first, get formulas and quantities *)
                 (* print_endline ("Expanding ingredient " ^ a); *)
                 let a_formula = Hashtbl.find cookbook a in
                 let get_quantity ingredient = 
                   (match Hashtbl.find_opt fuel_formula ingredient with
                     | Some q -> q
                     | None -> 0.) in
                 let needed_qt = get_quantity a in
                 Hashtbl.remove fuel_formula a;
                 if needed_qt = 0. then ( 
                   print_endline "Nothing to do, moving to next ingredient";
                   expand tl) (* a is not used in the fuel formula, move on *)
                 else (
                   let output_qt = Hashtbl.find a_formula "output" in
                   let mult_qt = ( *. ) (needed_qt /. output_qt) in
                   Hashtbl.remove a_formula "output";
                   (* iterate over a's ingredients and add them to the fuel formula *)
                   let add_ingredient ing qt =
                     let prev_qt = get_quantity ing in
                     let new_qt = prev_qt +. (mult_qt qt) in
                     Hashtbl.replace fuel_formula ing new_qt; in
                   Hashtbl.iter add_ingredient a_formula;
                   (* print_endline ("Fuel formula after expansion: " ^ (string_of_formula fuel_formula)); *)
                   expand tl) in
  let expansion_order = cookbook
    |> dependency_map
    |> topological_sort
    |> List.tl (* topological sort starts with "FUEL" which shouldn't be expanded *) in
  expand expansion_order

let part1 file =
  file
    |> Utils.read_lines
    |> create_cookbook
    |> expand_fuel_formula
    |> string_of_formula
    |> print_endline


let part2 file = 
  let formula = file
    |> Utils.read_lines
    |> create_cookbook
    |> expand_fuel_formula in
  let req_ore = Hashtbl.find formula "ORE" in
  1000000000000. /. req_ore


let _ = ["day14ex1.dat"; "day14ex2.dat"; "day14.dat"]
  |> List.map part2 
  |> List.map string_of_float
  |> List.map print_endline