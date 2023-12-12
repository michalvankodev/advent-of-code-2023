open Core

type direction = Left | Right | Top | Bottom [@@deriving equal]

let is_left direction = 
  match direction with
  | Left -> true
  | _ -> false
  
let is_right direction = 
  match direction with
  | Right -> true
  | _ -> false

let is_top direction = 
  match direction with
  | Top -> true
  | _ -> false

let is_bottom direction = 
  match direction with
  | Bottom -> true
  | _ -> false

let explode_string s = List.init (String.length s) ~f:(String.get s) 

let find_start map =
  let (y, x) = List.find_mapi_exn map ~f:(fun y row ->
  let x = List.find_mapi row ~f:(fun x char -> if Char.equal char 'S' then Some(x) else None) in
  if Option.is_some x then Some((y, Option.value_exn x)) else None
  ) in
  (y, x)

let get_point_value map (y, x) = List.nth_exn (List.nth_exn map y) x

let top_valid_positions = ['|'; '7'; 'F']
let right_valid_positions = ['-'; '7'; 'J']
let bottom_valid_positions = ['|'; 'J'; 'L']
(* let left_valid_positions = ['-'; 'F'; 'L'] *)

let next_step_for_start map start = 
  let (start_y, start_x) = start in
  let top_pos = get_point_value map ((start_y - 1), start_x) in
  (*let left_pos = get_point_value map (start_y, (start_x - 1)) in *)
  let right_pos = get_point_value map (start_y, (start_x + 1)) in
  let bottom_pos = get_point_value map ((start_y + 1), start_x) in
  if start_y <> 0 && List.exists top_valid_positions ~f:(fun c -> Char.equal c top_pos) then ((start_y - 1), start_x, Bottom)
  else if List.exists right_valid_positions ~f:(fun c -> Char.equal c right_pos) then (start_y, start_x + 1, Left)
  (* else if List.exists left_valid_positions ~f:(fun c -> Char.equal c left_pos) then (start_y, start_x - 1, Right) *)
  else if List.exists bottom_valid_positions ~f:(fun c -> Char.equal c bottom_pos) then ((start_y + 1), start_x, Top)
  else (start_y, start_x, Left)

let play_game_step map (y, x, prev_pos) = 
  let pipe_value = get_point_value map (y, x) in
  match pipe_value with 
  | '|' when is_bottom prev_pos  -> (y - 1, x, Bottom)
  | '|' when is_top prev_pos -> (y + 1, x, Top)
  | '-' when is_left prev_pos -> (y, x + 1, Left)
  | '-' when is_right prev_pos -> (y, x - 1, Right)
  | '7' when is_bottom prev_pos -> (y, x - 1, Right)
  | '7' when is_left prev_pos -> (y + 1, x, Top)
  | 'L' when is_right prev_pos -> (y - 1, x, Bottom)
  | 'L' when is_top prev_pos  -> (y, x + 1, Left)
  | 'F' when is_bottom prev_pos -> (y, x + 1, Left)
  | 'F' when is_right prev_pos  -> (y + 1, x, Top)
  | 'J' when is_top prev_pos -> (y, x - 1, Right)
  | 'J' when is_left prev_pos -> (y - 1, x, Bottom)
  | _ -> raise (Failure "you dumb")
 
let rec play_game map traveled steps curr_pos =
  let next_pos = play_game_step map curr_pos in
  let (y, x) = (fst3 next_pos, snd3 next_pos) in
  let next_point_val = get_point_value map (y, x) in
  print_endline (Printf.sprintf "steps: %d, next_point_val %d, %d" steps y x);
  let point_string = Printf.sprintf "(%d, %d)" y x in
  let traveled_next = Set.add traveled point_string in
  if Char.equal 'S' next_point_val then (steps + 1, traveled) else
  play_game map traveled_next (steps + 1) next_pos

let count_in_loop_area (map: char list list) traveled = 
  List.foldi map ~init:0 ~f:(fun test_y count row -> 
    let (row_count, _switch) = List.foldi row ~init:(0, false) ~f:(fun test_x (row_count, switch) point_value -> 
      let point_string = Printf.sprintf "(%d, %d)" test_y test_x in
      let is_traveled = Set.exists traveled ~f:(fun a -> String.equal a point_string) in 
      print_endline (Printf.sprintf "encounter %c, is_traveled: %b switch: %b row_count: %d" point_value is_traveled switch row_count);
      if is_traveled then 
          (match point_value with
          | 'L' | 'J' | '|' -> (row_count, not switch)
          | _ -> (row_count, switch))
       else
       (match switch with 
        | true -> (row_count + 1, switch)
        | false -> (row_count, switch))
      ) in
    count + row_count
  )

let read_file filename =
  try
    let map = ref [] in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
        let row = explode_string line in
        map := List.append !map [row];
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file;
    let start = find_start !map in
    let find_next_step = next_step_for_start !map start in 
    print_endline (Printf.sprintf "start at: %d, %d" (fst start) (snd start));
    print_endline (Printf.sprintf "starting at: %d, %d" (fst3 find_next_step) (snd3 find_next_step));
    let next_step_string = Printf.sprintf "(%d, %d)" (fst3 find_next_step) (snd3 find_next_step) in    let start_string = Printf.sprintf "(%d, %d)" (fst start) (snd start) in
    let empty_set = Set.of_list (module String) [start_string; next_step_string] in
    let (steps, traveled) = play_game !map empty_set 1 find_next_step in
    print_endline (Printf.sprintf "steps: %d, steps.halved %d" steps (steps / 2));
    (* TODO change S char in map to actual tile *)
    let in_loop_count = count_in_loop_area !map traveled in
    print_endline (Printf.sprintf "in_loop_count: %d, steps.halved %d" in_loop_count (steps / 2));

    with Sys_error msg ->
  print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
