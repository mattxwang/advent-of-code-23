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

module IS = Set.Make(Int);;

let split_into_ints line =
  let spl = Str.split (Str.regexp " +") line in
  map int_of_string spl

let process_line line =
  let r = Str.regexp {|Card[ ]*\([0-9]+\): \(.*\) | \(.*\)|} in
  let _ = Str.string_match r line 0 in
  let id = int_of_string (Str.matched_group 1 line) in
  let winning = (Str.matched_group 2 line) in
  let yours = (Str.matched_group 3 line) in
  let winning_set = IS.of_list (split_into_ints winning) in
  let your_set = IS.of_list (split_into_ints yours) in
  (id, winning_set, your_set)

(* exponentiation by squaring - see https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let line_to_points (_, winning, your) =
  let inter = IS.inter winning your in
  IS.cardinal inter

let floor_exp x = if x = 0 then 0 else pow 2 (x - 1)

let rec sumL = function
  | [] -> 0
  | hd :: tl -> hd + sumL tl

(* not tail recursive *)
let rec add_until lst = function
  | 0 -> lst
  | n -> match lst with
    | [] -> []
    | (num, pt) :: tl -> (num + 1, pt) :: add_until tl (n - 1)

let rec add_until_x_times lst pt = function
  | 0 -> lst
  | n -> add_until_x_times (add_until lst pt) pt (n - 1)

let card_pass points =
  let zipped = init (length points) (fun x -> (1, nth points x)) in
  let rec helper accum = function
    | [] -> accum
    | (num, pt) :: tl -> helper (accum + num) (add_until_x_times tl pt num)
  in helper 0 zipped


let () =
  let raw_lines = read_file "input.txt" in
  let processed = map process_line raw_lines in
  (* part one *)
  let inter_size = map line_to_points processed in
  let points_1 = map floor_exp inter_size in
  print_int (sumL points_1);
  print_string "\n";
  (* part two *)
  let points_two = card_pass inter_size in
  print_int points_two;
  print_string "\n"
