open Core

let explode_string s = List.init (String.length s) ~f:(String.get s) 


let get_empty_rows map = 
  List.foldi map ~init:[] ~f:(fun index acc row ->
    let has_galaxy = List.exists row ~f:(fun char -> Char.equal '#' char) in
    if has_galaxy then acc
    else acc @ [index]
  )

let get_empty_columns map =
  let length = List.hd_exn map |> List.length in
  let range = List.range 0 (length - 1) in
  let columns_without_galaxy = List.fold range ~init:[] ~f:(fun acc x -> 
    let columns_at_x = List.map map ~f:(fun row -> List.nth_exn row x) in
    let has_galaxy = List.exists columns_at_x ~f:(fun char -> Char.equal '#' char) in
    if has_galaxy then acc else List.append acc [x]
  ) in
columns_without_galaxy

let get_galaxy_positions map = 
  List.foldi map ~init:[] ~f:(fun y galaxies row -> 
    print_endline "";
    let row_galaxies = List.foldi row ~init:[] ~f:(fun x row_galaxies char -> 
    print_string(Printf.sprintf "%c" char);
      if Char.equal char '#' then row_galaxies @ [x]
      else row_galaxies
    ) in
    let galaxy_pos = List.map row_galaxies ~f:(fun row_pos -> (y, row_pos)) in
    galaxies @ galaxy_pos
  )

let get_between_space a b empty multiplier =
  if a = b then 0
  else
  let is_a_lower = a < b in
  let range = match is_a_lower with 
   | true -> List.range a b
   | false -> List.range b a
  in 
  let empty_columns_crossed = List.filter empty ~f:(fun e ->
     print_endline (Printf.sprintf "empty %d" e);
     List.exists range ~f:(fun r -> r = e)) in
  let empty_length = List.length empty_columns_crossed in
  print_endline (Printf.sprintf "empty_length %d" empty_length);
  (empty_length * multiplier) - empty_length + Int.abs(b - a)  
  
let get_total_length map empty_rows empty_columns empty_multiplier = 
  let galaxies = get_galaxy_positions map in
  let galaxy_pairs = List.foldi galaxies ~init:[] ~f:(fun index pairs galaxy ->
    let rest_galaxies = List.drop galaxies (index + 1) in 
    let g_pairs = List.map rest_galaxies ~f:(fun paired_galaxy -> (galaxy, paired_galaxy)) in
    pairs @ g_pairs
  ) in
  let distances = List.map galaxy_pairs ~f:(fun ((a_y, a_x), (b_y, b_x)) -> 
  let y_distance = get_between_space a_y b_y empty_rows empty_multiplier in 
  let x_distance = get_between_space a_x b_x empty_columns empty_multiplier in
    print_endline (Printf.sprintf "A: (%d, %d) B: (%d, %d) y_distance: %d,x-distance: %d" a_y a_x b_y b_x y_distance x_distance);
    y_distance + x_distance
  ) in
  List.fold distances ~init:0 ~f:(fun acc distance -> acc + distance)



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

    let empty_rows = get_empty_rows !map in
    let empty_columns = get_empty_columns !map in 
    let total_length = get_total_length !map empty_rows empty_columns 1000000 in
    
    print_endline (Printf.sprintf "Total length : %d" total_length);
    with Sys_error msg ->
  print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
