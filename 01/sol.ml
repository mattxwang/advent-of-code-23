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

let string_to_char_list s =
  s |> String.to_seq |> of_seq

let rec first_digit str =
  match str with
    | [] -> "a" (* good job matt very robust*)
    | ('0'..'9') as x :: tl -> Char.escaped x
    | _ :: tl -> first_digit tl

let last_digit str = first_digit (rev str)

let calibration str = int_of_string (first_digit str ^ last_digit str)

let rec sumL = function
  | [] -> 0
  | hd :: tl -> hd + sumL tl

let () =
  let raw_lines = read_file "input.txt" in
  let char_lines = map string_to_char_list raw_lines in
  let calibrations = map calibration char_lines in
  let sum = sumL calibrations in
  print_int sum;
  print_string "\n"
