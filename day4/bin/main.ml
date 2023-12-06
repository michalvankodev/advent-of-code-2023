open Core

let get_game_id_from_game_part str = 
 let open Base in
 let parts = String.split str ~on:' ' in
 List.last_exn parts

let parse_numbers str = 
 let parts = String.split str ~on:' ' in
 List.fold parts ~f:(fun numbers string -> 
   let convert = int_of_string_opt string in
   match convert with 
   | Some(number) -> List.append numbers [number]
   | None -> numbers 
 ) ~init:[]
 
let parse_line line =
 let parts = String.split line ~on:':' in
 let game_id_part = List.hd_exn parts in
 let game_id = int_of_string (get_game_id_from_game_part game_id_part) in
 let number_parts = String.split (List.last_exn parts) ~on:'|' in
 let winning_numbers_str = List.hd_exn number_parts in
 let my_numbers_str = List.last_exn number_parts in
 let winning_numbers = parse_numbers winning_numbers_str in
 let my_numbers = parse_numbers my_numbers_str in
 (game_id, winning_numbers, my_numbers)


let read_file filename =
  try
    let card_copies = ref (Map.empty (module Int)) in
    let originals = ref 0 in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
        print_endline (Printf.sprintf "%s" line);
        let (game_id, winning_numbers, my_numbers) = parse_line line in
        let matched_numbers = List.count my_numbers ~f:(fun num -> List.exists winning_numbers ~f:(fun wn -> wn = num)) in 
        originals := !originals + 1;
        if matched_numbers > 0 then (
        let current_game_copies = Option.value (Map.find !card_copies game_id) ~default:0 + 1 in
        let my_range = List.range (game_id + 1)  (game_id + matched_numbers + 1) in
        List.iter my_range ~f:(fun game_copy_id ->
          let game_copies = match Map.find !card_copies game_copy_id with
           | Some(value) -> value + current_game_copies
           | None -> current_game_copies
          in
          card_copies := Map.set !card_copies ~key:game_copy_id ~data:game_copies; 
        );
        )
        else ()
      | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file; 
    let total_card_copies = Map.fold !card_copies ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) in
    print_endline (Printf.sprintf "Total number of cards: %d" (total_card_copies + !originals));
  with Sys_error msg ->
    print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
