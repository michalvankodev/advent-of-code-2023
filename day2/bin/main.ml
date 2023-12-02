
let get_game_id_from_game_part str = 
 let open Base in
 let parts = String.split str ~on:' ' in
 let game_id_str = List.last parts in
 match game_id_str with 
  | Some(id) -> Int.of_string id
  | None -> 0  

let check_invalid_color str =
 let open Base in
 let split = String.split (String.strip str) ~on:' ' in
 let balls_amount = List.hd_exn split  |> String.strip |> Int.of_string in
 let balls_color = List.hd_exn (List.tl_exn split) |> String.strip in
 match balls_color with 
 | "red" -> balls_amount > 12 
 | "green" -> balls_amount > 13 
 | "blue" -> balls_amount > 14 
 | _ -> true

let is_game_valid str = 
 let open Base in
    Stdio.print_endline (Printf.sprintf "gameString: %s" str);
  let ball_colors = String.split str ~on:',' in
  not (List.exists ball_colors ~f:check_invalid_color)

(* for each game we need to get the values of red blue and green balls *)
let get_amount_for_color str = 
 let open Base in
 let split = String.split (String.strip str) ~on:' ' in
 let balls_amount = List.hd_exn split  |> String.strip |> Int.of_string in
 let balls_color = List.hd_exn (List.tl_exn split) |> String.strip in
 (balls_color, balls_amount)

let get_game_colors game =
 let open Base in
 let ball_colors = String.split game ~on:',' in
 List.map ball_colors ~f:get_amount_for_color

(* then we need to find max of those values from the games *)
let get_game_power games =
 let all_games_colors = List.flatten (List.map get_game_colors games) in
 let (r, g, b) = List.fold_left (fun (r_max, g_max, b_max) game -> 
   match game with
  | ("red", amount) when amount > r_max -> (amount, g_max, b_max) 
  | ("green", amount) when amount > g_max -> (r_max, amount, b_max) 
  | ("blue", amount) when amount > b_max -> (r_max, g_max, amount) 
  | _ -> (r_max, g_max, b_max)
  ) (0, 0, 0) all_games_colors in
 r * g * b
 

let parse_line line = 
 let open Base in
 let parts = String.split line ~on:':' in
 let game_part = List.hd parts in
 let game_id = match game_part with
 | Some(str) -> get_game_id_from_game_part str
 | None -> 0 in
if game_id = 0 then 0
else 
let games = List.tl parts in 
let game_strings_together = match games with 
 | Some(list_of_games) -> List.hd_exn(list_of_games)
 | None -> "" in
let game_strings = String.split game_strings_together ~on:';' in
let _games_valid = List.map game_strings ~f:is_game_valid in
let _game_id_if_valid = if List.exists _games_valid ~f:(fun is_valid -> not is_valid) then 0
else game_id in
get_game_power(game_strings)


let read_file filename =
  try
    let total = ref 0 in
    let file = open_in filename in
    try
      while true do
        let line = input_line file in
        (* Process the line as needed *)
        print_endline (Printf.sprintf "%s" line);
        let game_power = parse_line line in
        total := !total + game_power;
        print_endline (Printf.sprintf "latest game power:%d, Total: %d" game_power !total);
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
