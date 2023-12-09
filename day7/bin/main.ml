open Core

let to_int c =
  if Char.is_digit c then
    Char.to_int c - Char.to_int '0'
  else 
    0
(*
type hand_type = 
  | FiveOfKind
  | FourOfKind
  | FullHouse
  | ThreeOfKind
  | TwoPair
  | OnePair
  | HighCard
*)

type hand = {
  cards: string;
  score: int;
  bid: int;
}

let explode_string s = List.init (String.length s) ~f:(String.get s) 

let get_hand_type_score (cards: string) = 
  let empty = Map.empty (module Char) in
  let card_chars = explode_string cards in
  let jokers_count = List.fold card_chars ~init:0 ~f:(fun acc char -> 
    if Char.equal char 'J' then acc + 1 else acc
  ) in
  let card_map = List.fold card_chars ~init:empty ~f:(fun acc char ->
    if not (Char.equal char 'J') then
    Map.change acc char ~f:(fun value ->
      (match value with
        | Some count -> Some(count + 1)
        | None -> Some 1
      )) else acc
    ) in
  let card_counts = Map.data card_map in
  let sorted_card_counts = List.sort card_counts ~compare:(fun a b -> b - a) in
  let with_jokers = [Option.value (List.hd sorted_card_counts) ~default:0 + jokers_count] @ Option.value (List.tl sorted_card_counts) ~default:[] in
  match with_jokers with
  | [5] -> 7
  | [4; 1] -> 6
  | [3; 2] -> 5
  | [3; 1; 1] -> 4
  | [2; 2; 1] -> 3
  | [2; 1; 1; 1] -> 2
  | _ -> 1

 
let parse_hand line = 
  let parts = String.split line ~on:' ' in
  match parts with 
  | [cards; bid_s] -> 
    let bid = int_of_string(bid_s) in
    let score = get_hand_type_score cards in
    { cards; bid; score }
  | _ -> raise End_of_file 

let get_card_score char = 
  match char with 
   | 'A' -> 14 
   | 'K' -> 13 
   | 'Q' -> 12 
   | 'J' -> 1 
   | 'T' -> 10 
   | n when Char.is_digit n -> to_int(n)
   | _ -> 0

let compare_card_values cards_a cards_b = 
  let a_scores = explode_string cards_a |> List.map ~f:get_card_score in
  let b_scores = explode_string cards_b |> List.map ~f:get_card_score in
  let card_pairs_scores = List.zip_exn a_scores b_scores in
  let (diff_a, diff_b) = List.find_exn card_pairs_scores ~f:(fun (a, b) -> 
     a <> b
  ) in
  (diff_a - diff_b)
  
let read_file filename =
  try
    let hands = ref [] in
    let file = Stdio.In_channel.create filename in
    try
      while true do
        let line = Stdio.In_channel.input_line file in
        match line with 
        | Some(line) ->
        (* Process the line as needed *)
          let hand = parse_hand line in
          hands := List.append !hands [hand];
        | None -> raise End_of_file
    done;
    with End_of_file ->
    Stdio.In_channel.close file; 
    let sorted_hands = List.sort !hands ~compare:(fun a b -> 
    if a.score = b.score then
      compare_card_values a.cards b.cards
    else a.score - b.score
   ) in
    let total_score = List.foldi sorted_hands ~init:0 ~f:(fun index acc hand ->
      print_endline (Printf.sprintf "Cards: %s, Rank: %d, Score: %d" hand.cards index hand.score);
      acc + (hand.bid * (index + 1))
    ) in
    print_endline (Printf.sprintf "Total: %d" total_score);
  with Sys_error msg ->
    print_endline (Printf.sprintf "Error: %s\n" msg)


let () =
 let argvs = Sys.get_argv() in
 if Array.length argvs <> 2 then
    print_endline (Printf.sprintf "Usage: %s <filename>\n" argvs.(0))
  else
    read_file argvs.(1)
