open Core

let explode_string s = List.init (String.length s) ~f:(String.get s) 

let all_except_last lst =
  lst |> List.rev |> List.tl_exn |> List.rev
 
let get_records str =
  let line_parts = String.split ~on:',' str in
  List.map line_parts ~f:int_of_string

let get_sequence chars = List.fold chars ~init:[] ~f:(fun acc char -> 
    let last_char = List.last acc in
    match last_char with 
    | Some((l_ch, times)) -> 
      if Char.equal char l_ch then (all_except_last acc) @ [(char, times + 1)]
      else acc @ [(char, 1)]
    | None -> [(char, 1)]
  )

let get_sequence_from_str str =
  let chars = explode_string str in
  get_sequence chars

let seq_to_str (char, times) =
 List.init times ~f:(fun _ -> char)

let single_combi = [['#'];['.']] 

let rec get_combinations times =
  if times = 1 then single_combi
  else let combinations = List.concat_map single_combi ~f:(fun char ->
   List.concat_map char ~f:(fun ch ->
   let rest_combs = get_combinations (times - 1) in
   let combs = List.map rest_combs ~f:(fun rest -> [ch] @ rest) in
   combs
 )) in
 combinations
    
let get_all_possible_sequences sequence =
  let possible_sequences = List.map sequence ~f:(fun (char, times) ->
    match char with
      | '?' -> get_combinations times
      | _ -> [seq_to_str (char, times)]
  ) in
  List.fold possible_sequences ~init:[[]] ~f:(fun acc seqs -> 
    let new_acc = List.concat_map acc ~f:(fun seq_until -> List.map seqs ~f:(fun seq ->

      print_endline (Printf.sprintf "building: %s+%s" (String.of_char_list seq_until) (String.of_char_list seq));
     seq_until @ seq))

   in new_acc
  )

let is_valid_sequence records sequence = 
  print_endline (Printf.sprintf "");
  let operational = List.filter sequence ~f:(fun (char, _times) ->
        (*print_endline (Printf.sprintf "checking for validity char: %c, times: %d" char times); *)
     Char.equal char '#') in
  let operational_times = List.map operational ~f:snd in
  List.equal (fun a b ->
    print_string (Printf.sprintf "(%d, %d)" a b);
   a = b) operational_times records 

let get_row_possibilities line =
  let line_parts = String.split ~on:' ' line in
  match line_parts with 
  | [spring_part; record_part] -> 
    let records = get_records record_part in
    let sequence = get_sequence_from_str spring_part in
    let all_possible_sequences_str = get_all_possible_sequences sequence in
    let all_possible_sequences = List.map all_possible_sequences_str ~f:get_sequence in
    let valid_sequences = List.count all_possible_sequences ~f:(is_valid_sequence records) in
    valid_sequences
  | _ -> 0 

let read_file filename =
  try
    let file = Stdio.In_channel.create filename in
    let total_sum = ref 0 in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        let row_possibilities = get_row_possibilities line in 
        total_sum := !total_sum + row_possibilities;
        print_endline (Printf.sprintf "line %s, row_possibilities %d" line row_possibilities);
        (* Process the line as needed *)
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file;
    
    print_endline (Printf.sprintf "Total posibilities %d" !total_sum);
    with Sys_error msg ->
  print_endline (Printf.sprintf "Error: %s\n" msg)

let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
