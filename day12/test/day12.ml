open Core 

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


let () =
  let test1 = get_combinations 4 in
  let _map = List.map test1 ~f:(fun row ->
    let _ = List.map row ~f:(fun char ->
        print_string (Printf.sprintf "%c" char);
        char
    ) in
  print_endline (Printf.sprintf "");
  ()
 ) in
  ()


