open Core

let get_records str =
  let line_parts = String.split ~on:',' str in
  List.map line_parts ~f:int_of_string

let copy_list times list =
  List.concat (List.init times ~f:(fun _ -> list))

let copy_string times (str: string) =
  let list = List.init times ~f:(fun _ -> str) in
  String.concat list ~sep:"?"

let cache = Hashtbl.create (module String)

let key_to_string (records, cfg) =
  let records_string = List.map records ~f:(fun i -> string_of_int i) in
  let rec_str = String.concat records_string ~sep:"," in
  rec_str ^ ":" ^ cfg

let rec count records cfg =
  if String.is_empty cfg then if List.is_empty records then 1 else 0
  else if List.is_empty records then if String.contains cfg '#' then 0 else 1
  else
  match Hashtbl.find cache (key_to_string (records, cfg)) with
  | Some(value) -> value
  | _ ->
  let result = ref 0 in
  let first = String.nget cfg 0 in
  let _ = if (List.exists ['?'; '.'] ~f:(fun c -> Char.equal c first)) then result := (!result + count records (String.slice cfg 1 0)) in
  let _ = if (List.exists  ['?'; '#']~f:(fun c -> Char.equal c first)) then 
    let first_rec = List.hd_exn records in
    if first_rec <= String.length cfg && not (String.contains (String.slice cfg 0 first_rec) '.') &&
    (first_rec = String.length cfg || not (Char.equal (String.nget cfg first_rec) '#')) then result := !result + count (List.tl_exn records) (String.slice cfg (first_rec +1) 0);
  in
    Hashtbl.add_exn cache ~key:(key_to_string (records, cfg)) ~data:!result;
    !result

let get_row_possibilities line = 
  let line_parts = String.split ~on:' ' line in
  match line_parts with 
  | [spring_part; record_part] -> 
    let records = get_records record_part |> copy_list 5 in
    let count = count records ((spring_part |> copy_string 5) ^ ".") in
    count
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
