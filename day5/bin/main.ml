open Core

type mapping_type = 
    | Mapped  
    | NotMapped

let is_between x lower_bound upper_bound =
  x >= lower_bound && x <= upper_bound

let parse_numbers str = 
 let parts = String.split str ~on:' ' in
 List.fold parts ~f:(fun numbers string -> 
   let convert = int_of_string_opt string in
   match convert with 
   | Some(number) -> List.append numbers [number]
   | None -> numbers 
 ) ~init:[]

let parse_seeds line =
  let parts = String.split line ~on:':' in
  let seeds_part = List.last_exn parts in
  let seeds = parse_numbers seeds_part in
  seeds


let get_next_type line (source_mapping: (int * mapping_type)) = 
  let (source, mapping) = source_mapping in
  match mapping with
    | Mapped -> source_mapping
    | NotMapped ->
  let destination_map = parse_numbers line in
  match destination_map with
  | [destination_start; source_start; range] -> (
      if is_between source source_start (source_start + range) then
        (destination_start + source - source_start, Mapped)
      else source_mapping
    )
  | _ -> source_mapping 

let reset_mapping source_mapping =
  let (source, _mapping) = source_mapping in
  (source, NotMapped)
  
let play_game source_mappings line = 
  match line with
  | l when String.is_substring ~substring:"seeds" l ->
      let seeds = parse_seeds line in
      List.map seeds ~f:(fun seed -> (seed, NotMapped))
  | l when String.contains l ':' -> List.map source_mappings ~f:reset_mapping
  | _ -> List.map source_mappings ~f:(get_next_type line)
 

let read_file filename =
  try
    let source_mappings = ref [] in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
          source_mappings := play_game !source_mappings line;
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file; 
    let (first_source_value, _map) = List.hd_exn !source_mappings in
    let lowest_source = List.fold !source_mappings ~init:first_source_value ~f:(fun acc (value, _) -> if value < acc then value else acc) in
    print_endline (Printf.sprintf "Lowest ending source: %d" lowest_source);
  with Sys_error msg ->
    print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
