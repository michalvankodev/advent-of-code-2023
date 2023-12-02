(* open Base *)
(* open Core *)

let to_int c =
  let open Base in
  if Char.is_digit c then
    Char.to_int c - Char.to_int '0'
  else 
    0

let explode_string s = List.init (String.length s) (String.get s) 

let rec transform_words line = 
  let open Base in
  let index_map = [|
    String.substr_index line ~pattern:"one";
    String.substr_index line ~pattern:"two";
    String.substr_index line ~pattern:"three";
    String.substr_index line ~pattern:"four";
    String.substr_index line ~pattern:"five";
    String.substr_index line ~pattern:"six";
    String.substr_index line ~pattern:"seven";
    String.substr_index line ~pattern:"eight";
    String.substr_index line ~pattern:"nine";
  |]  in
  let (min_val, min_idx, ________) =
    Array.fold ~init:(1000000, -1, 0) ~f:(fun (min_val, min_idx, curr_idx) index ->
        match index with
        | Some i when i < min_val -> (i, curr_idx, curr_idx+1)
        | Some _  | None -> (min_val, min_idx, curr_idx + 1)
      ) index_map in
  Stdio.print_endline (Printf.sprintf "minval %d, min_idx %d" min_val min_idx);
  Stdio.print_endline (Printf.sprintf "rest of line %s" line);
  if min_idx = -1 then line else
  let swap_map = [|
    (fun s -> String.substr_replace_first s ~pattern:"one" ~with_:"o1e");
    (fun s -> String.substr_replace_first s ~pattern:"two" ~with_:"t2o");
    (fun s -> String.substr_replace_first s ~pattern:"three" ~with_:"t3e");
    (fun s -> String.substr_replace_first s ~pattern:"four" ~with_:"f4r");
    (fun s -> String.substr_replace_first s ~pattern:"five" ~with_:"f5e");
    (fun s -> String.substr_replace_first s ~pattern:"six" ~with_:"s6x");
    (fun s -> String.substr_replace_first s ~pattern:"seven" ~with_:"s7n");
    (fun s -> String.substr_replace_first s ~pattern:"eight" ~with_:"e8t");
    (fun s -> String.substr_replace_first s ~pattern:"nine" ~with_:"n9e");
  |] in
  transform_words (swap_map.(min_idx) line)

let process_line line =
  let transformed = transform_words line in
  let line_int = explode_string transformed |> List.map to_int in
  List.fold_left (fun (f, s) x -> 
    if x > 0 && f = 0 then (x, x)  
    else if x > 0 && f <> 0 then (f, x)
    else (f, s)
  ) (0, 0) line_int
  
let read_file filename =
  try
    let total = ref 0 in
    let file = open_in filename in
    try
      while true do
        let line = input_line file in
        (* Process the line as needed *)
        let (f, l) = process_line line in
        let sum = f * 10 + l in
        total := !total + sum;

        print_endline (Printf.sprintf "line sum %d, total %d" sum !total);
      done
    with End_of_file ->
      close_in file
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg

let () =
 if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    read_file Sys.argv.(1)
