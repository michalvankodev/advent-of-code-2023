open Core

let parse_numbers line = 
  let parts = String.split line ~on:' ' in
  List.map parts ~f:int_of_string

let get_diff_between_numbers numbers = 
  let (diff_row, _) = List.fold numbers ~init:([], None) ~f:(fun (result, previous) current ->
  match previous with 
  | None -> (result, Some(current))
  | Some(prev_number) -> 
   let next_diff = current - prev_number in
   let next_result = List.append result [next_diff] in
   (next_result, Some(current)) 
 ) in
 diff_row

let all_same row  =
  List.all_equal row ~equal:(fun a b -> a = b)
 
let rec get_prediction row = 
  let first_number =  (List.hd_exn row) in
  let diff_row = get_diff_between_numbers row in
  if Option.is_some(all_same diff_row) then first_number - (List.hd_exn diff_row)
  else first_number - get_prediction diff_row

let predict_next_number line =
  let row = parse_numbers line in
  get_prediction row

let read_file filename =
  try
    let total_sum = ref 0 in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
        let next_number = predict_next_number line in
        total_sum := !total_sum + next_number;
        print_endline (Printf.sprintf "input: %s, prediction: %d, total: %d" line next_number !total_sum);
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file;
    with Sys_error msg ->
  print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
