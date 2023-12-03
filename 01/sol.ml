open List
(* open Str *)

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

let string_to_char_list s =
  s |> String.to_seq |> of_seq

let rec first_digit str =
  match str with
    | [] -> "a" (* good job matt very robust*)
    | ('0'..'9') as x :: tl -> Char.escaped x
    | _ :: tl -> first_digit tl

let last_digit str = first_digit (rev str)

let calibration str = int_of_string (first_digit str ^ last_digit str)

(*  surely the worst ocaml code i've ever written? *)
(*  unfort, the regex version doesn't work with things like "twoone" *)
let rec first_digit_with_strs str =
  match str with
    | [] -> "a" (* good job matt very robust*)
    | ('0'..'9') as x :: tl -> Char.escaped x
    | 'o' :: 'n' :: 'e'               :: _ -> "1"
    | 't' :: 'w' :: 'o'               :: _ -> "2"
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> "3"
    | 'f' :: 'o' :: 'u' :: 'r'        :: _ -> "4"
    | 'f' :: 'i' :: 'v' :: 'e'        :: _ -> "5"
    | 's' :: 'i' :: 'x'               :: _ -> "6"
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> "7"
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> "8"
    | 'n' :: 'i' :: 'n' :: 'e'        :: _ -> "9"
    | _ :: tl -> first_digit_with_strs tl

let rec last_digit_with_strs str =
  let rec helper str =
    match str with
      | [] -> "a" (* good job matt very robust*)
      | ('0'..'9') as x :: tl -> Char.escaped x
      | 'e' :: 'n' :: 'o'               :: _ -> "1"
      | 'o' :: 'w' :: 't'               :: _ -> "2"
      | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> "3"
      | 'r' :: 'u' :: 'o' :: 'f'        :: _ -> "4"
      | 'e' :: 'v' :: 'i' :: 'f'        :: _ -> "5"
      | 'x' :: 'i' :: 's'               :: _ -> "6"
      | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> "7"
      | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> "8"
      | 'e' :: 'n' :: 'i' :: 'n'        :: _ -> "9"
      | _ :: tl -> helper tl
  in helper (List.rev str)

let calibration_with_strs str = int_of_string (first_digit_with_strs str ^ last_digit_with_strs str)

let rec sumL = function
  | [] -> 0
  | hd :: tl -> hd + sumL tl

let () =
  let raw_lines = read_file "input.txt" in
  (* part one *)
  let char_lines = map string_to_char_list raw_lines in
  let calibrations = map calibration char_lines in
  let sum = sumL calibrations in
  (* part two *)
  let calibrations_with_strs = map calibration_with_strs char_lines in
  let sum_with_strs = sumL calibrations_with_strs in
  print_int sum;
  print_string "\n";
  print_int sum_with_strs;
  print_string "\n"
