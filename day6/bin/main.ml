open Core

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
  List.last_exn parts |> parse_numbers

let get_number_of_ways (time, distance) =
  let speeds = List.range 0 time in
  let distance_for_speeds = List.map speeds ~f:(fun speed -> 
    let left_over_time = time - speed in
    left_over_time * speed
  ) in
  List.count distance_for_speeds ~f:(fun speed_distance -> speed_distance > distance)
     

  

let read_file filename =
  try
    let times = ref [] in
    let distances = ref [] in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
          let values = parse_line line in
          if List.is_empty !times then (
            times := values;
          )
          else if List.is_empty !distances then (
            distances := values;
          )
          else ()
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file; 
    let games = List.zip_exn !times !distances in
    let number_of_ways = List.map games ~f:get_number_of_ways in
    let product = List.fold number_of_ways ~f:(fun acc n -> n * acc) ~init:1 in
    print_endline (Printf.sprintf "Number of ways: %d" product);
  with Sys_error msg ->
    print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
