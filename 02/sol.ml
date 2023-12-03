open List

(* STL read_file, from https://stackoverflow.com/a/5775024 *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
  ;;

type reveal = { red: int; green: int; blue: int }

let print_reveal { red; green; blue } =
  print_string ("R: " ^ string_of_int red);
  print_string ("G: " ^ string_of_int green);
  print_string ("B: " ^ string_of_int blue);
  print_string "\n"

let rec fd_with_str str = function
  | [] -> 0
  | hd :: tl ->
    let r = " ?\\([0-9]+\\) " ^ str in
    let show_regex = Str.regexp r in
    if Str.string_match show_regex hd 0
    then
      let num = (Str.matched_group 1 hd) in
      int_of_string num
    else fd_with_str str tl

let process_reveal reveal =
  let spl = Str.split (Str.regexp ",") reveal in
  let red = fd_with_str "red" spl in
  let green = fd_with_str "green" spl in
  let blue = fd_with_str "blue" spl in
  { red = red; green = green; blue = blue }

let process_game line =
  let raw_reveals = Str.split (Str.regexp ";") line in
  map process_reveal raw_reveals

let process_line line =
  let r = Str.regexp {|Game \([0-9]+\): \(.*\)|} in
  let _ = Str.string_match r line 0 in
  let id = int_of_string (Str.matched_group 1 line) in
  let game = process_game (Str.matched_group 2 line) in
  (id, game)

let validity { red; green; blue } =
  if red <= 12 && green <= 13 && blue <= 14 then 1 else 0

let rec prod_validity accum = function
  | [] -> accum
  | hd :: tl -> prod_validity (accum * validity hd) tl

let rec mult_validity accum = function
  | [] -> accum
  | (id, game) :: tl -> mult_validity (accum + (id * (prod_validity 1 game))) tl

let rec best_reveal { red; green; blue } = function
  | [] -> { red; green; blue }
  | {red = red_c; green = green_c; blue = blue_c} ::tl -> best_reveal {
    red = max red red_c;
    green = max green green_c;
    blue = max blue blue_c
  } tl

let reveal_power { red; green; blue } = red * green * blue

let rec sumL = function
  | [] -> 0
  | hd :: tl -> hd + sumL tl

let () =
  let raw_lines = read_file "input.txt" in
  let games = map process_line raw_lines in
  (*  part one *)
  let validities = mult_validity 0 games in
  print_int validities;
  print_string "\n";
  (* part two *)
  let best_reveals = map (best_reveal {red=0; blue=0; green=0}) (map snd games) in
  let powers = map reveal_power best_reveals in
  print_int (sumL powers);
  print_string "\n"
