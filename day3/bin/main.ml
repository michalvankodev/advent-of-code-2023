open Base
open Stdio

let to_int c =
  if Char.is_digit c then
    Char.to_int c - Char.to_int '0'
  else 
    0

let rec substitute_last lst new_last =
  match lst with
  | [] -> []
  | [_last] -> [new_last]  (* Replace the last element *)
  | head :: tail -> head :: substitute_last tail new_last

let explode_string s = List.init (String.length s) ~f:(String.get s) 

let _get_symbols line = 
  let chars = explode_string line in 
  List.filter_mapi chars ~f:(fun index char ->
    match char with 
     | '.' -> None
     | c when Char.is_digit c -> None 
     | _ -> Some(index)
  )

let get_gear_symbols line = 
  let chars = explode_string line in 
  List.filter_mapi chars ~f:(fun index char ->
    match char with 
     | '*' -> Some(index)
     | _ -> None
  )

type value_tuple = int * (int * int)

let get_numbers line = 
  let chars = explode_string line in 
  let (found, _index) = List.fold chars ~f:(fun (found, index) char ->
    match char with 
     | c when Char.is_digit c -> 
       (match List.last(found) with
         | Some((number, (start, ending))) when ending = index - 1 -> 
          let value: value_tuple = ((number * 10 + (to_int char)), (start, index)) in
          (substitute_last found value, index + 1) 
         | _ -> (List.append found [(to_int char, (index, index))], index + 1))
     | _ -> (found, index + 1)
  ) ~init:([], 0) in
    found

(*
let get_numbers line = 
  let chars = explode_string line in 
  List.fold chars ~f:(fun (found, index) char ->
    if Char.is_digit char then
      let last_found = List.last found in
      match last_found with
       | Some(tuple) -> (match tuple with
         | (number, position) -> ( 
      match position with 
       (start, ending) ->
          if (ending = index - 1) then
          let number = (number * 10) + (to_int char) in
          let value: value_tuple = (number, (start, index)) in
          ((List.of_list [value]), index + 1)
        else (List.append found [(to_int char, (index, index))], index + 1)
        | _ -> (found, index + 1))
      | _ -> (found, index + 1))
      | None -> (found, index + 1)
    else (found, index + 1)
  ) ~init:([], 0)
*)

let is_between x lower_bound upper_bound =
  x >= lower_bound && x <= upper_bound

let _split_touching_numbers symbols numbers = 
  List.partition_tf numbers ~f:(fun (_value, (start, ending)) ->
    List.exists symbols ~f:(fun symbol_index -> 
      is_between symbol_index (start -1) (ending + 1)
    )
  )
(*
let first_star_implementation symbol_positions number_positions = 
        let (current_line_numbers_with_current_symbols, current_line_umatched) = split_touching_numbers symbol_positions number_positions in
        let (current_line_numbers_with_prev_symbols, _current_line_ps_unmatched) = split_touching_numbers !prev_line_symbol_pos number_positions in
        let (prev_line_numbers_with_current_symbols, _) = split_touching_numbers symbol_positions !prev_line_number_pos in
        let counted_values = List.map (current_line_numbers_with_current_symbols @ current_line_numbers_with_prev_symbols @ prev_line_numbers_with_current_symbols) ~f:(fun (value, _) -> value) in
        let _sum_of_current = List.fold ~f:(fun a i ->  a + i) ~init:0 counted_values in
true
*)

let get_gears numbers symbols = 
  List.filter_map symbols ~f:(fun symbol_index -> 
  let touching_numbers = List.filter_map numbers ~f:(fun (value, (start, ending)) -> 
    if is_between symbol_index (start - 1) (ending + 1) then
    Some(value)
    else None
  ) in
  if List.length touching_numbers = 2 then
    let product = List.fold touching_numbers ~f:(fun a value -> a * value) ~init:1 in
    Some(product)
  else None
) 

let read_file filename =
  try
    let total = ref 0 in
    let prev_line_symbol_pos = ref (List.of_list []) in
    let prev_line_number_pos = ref (List.of_list []) in
    let prev_prev_line_number_pos = ref (List.of_list []) in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
        print_endline (Printf.sprintf "%s" line);
        let symbol_positions = get_gear_symbols line in
        let number_positions = get_numbers line in
        let concat_number_positions = !prev_prev_line_number_pos @ !prev_line_number_pos @ number_positions in
        let found_gears = get_gears concat_number_positions !prev_line_symbol_pos in
        let sum_of_current = List.fold found_gears ~f:(fun a value -> a + value) ~init:0 in
        prev_prev_line_number_pos := !prev_line_number_pos;
        prev_line_number_pos := number_positions;
        prev_line_symbol_pos := symbol_positions;
        total := !total + sum_of_current;
        print_endline (Printf.sprintf "latest game power:%d, Total: %d" sum_of_current !total);
      | None -> ()
      done
    with End_of_file ->
      Stdio.In_channel.close file
  with Sys_error msg ->
    print_endline (Printf.sprintf "Error: %s\n" msg)

let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
