open Core

let explode_string s = List.init (String.length s) ~f:(String.get s) 

let read_file filename =
  try
    let file = Stdio.In_channel.create filename in
    let map = ref [] in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        let row = explode_string line in 
        map := List.append !map [row]
        (* Process the line as needed *)
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file;

    
    print_endline (Printf.sprintf "Total result " );
    with Sys_error msg ->
  print_endline (Printf.sprintf "Error: %s\n" msg)

let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
