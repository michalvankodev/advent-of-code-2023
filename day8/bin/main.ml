open Core

let explode_string s = List.init (String.length s) ~f:(String.get s) 

let parse_steps str =
  let parts = String.split str ~on:',' in
  match parts with 
  | [left_part; right_part] -> 
    let left_step_split = String.split left_part ~on:'(' in
    let left_step = List.nth_exn left_step_split 1 in
    let right_step_split = String.split right_part ~on:')' in
    let right_step = String.strip (List.hd_exn right_step_split) in
    (left_step, right_step)
  | _ -> raise End_of_file 

let parse_map_node line = 
  let parts = String.split line ~on:'=' in
  match parts with 
  | [position_part; step_part] -> 
    let position = String.strip position_part in
    let (left_step, right_step) = parse_steps step_part in
    (position, (left_step, right_step))
  | _ -> raise End_of_file 

let is_starting_pos position =
  let ending_char = String.to_list_rev position |> List.hd_exn in
   Char.equal ending_char 'A'

let is_ending_pos position =
  let ending_char = String.to_list_rev position |> List.hd_exn in
   Char.equal ending_char 'Z'

(* let is_game_end positions =
   not (List.exists positions ~f:(fun pos -> not (is_ending_pos pos)))
*)

let read_file filename =
  try
    let travel_map = ref (Map.empty (module String)) in
    let steps = ref [] in
    let starting_positions = ref [] in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
        if List.is_empty !steps then
          steps := explode_string line
        else if String.is_empty line then ()
        else
          let (position, (left_step, right_step)) = parse_map_node line in
          if is_starting_pos position then starting_positions := List.append !starting_positions [position];
          travel_map := Map.add_exn !travel_map ~key:position ~data:(left_step, right_step);
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file;
    print_endline (Printf.sprintf "Number Starting positions: %d," (List.length !starting_positions));
    let cycle = Sequence.cycle_list_exn !steps in
    let _steps_for_pos = List.map !starting_positions ~f:(fun pos -> 
    let (total_steps, last_position) = Sequence.fold_until cycle ~init:(0, pos) ~finish:(fun (no_steps, curr_pos) -> (no_steps, curr_pos)) ~f:(fun (no_steps, curr_poss) direction ->
      if is_ending_pos curr_poss then Stop((no_steps, curr_poss))
      else
        let (left_position, right_position) = Map.find_exn !travel_map curr_poss in  
        let next_poss = match direction with
        | 'L' ->  left_position 
        | 'R' ->  right_position 
        | _ -> curr_poss
        in 
       Continue ((no_steps + 1, next_poss))
    ) in
    print_endline (Printf.sprintf "Total steps: %d, last_pos: %s" total_steps last_position);
    ) in
    print_endline (Printf.sprintf "the end");
    with Sys_error msg ->
  print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
