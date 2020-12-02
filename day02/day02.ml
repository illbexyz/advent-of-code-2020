open Base
open Stdio

let filepath = "day02/input.txt"

type range = { min : int; max : int; letter : char }

type password_with_conf = { range : range; password : string }

module Parser = struct
  open Angstrom

  let wss = take_while Char.is_whitespace

  let num_parser = Int.of_string <$> take_while1 Char.is_digit

  let range_parser : range t =
    (fun min max letter -> { min; max; letter })
    <$> wss *> num_parser
    <*> char '-' *> num_parser
    <*> (char ' ' *> any_char <* wss)

  let line_parser : password_with_conf t =
    (fun range password -> { range; password })
    <$> range_parser
    <*> (wss *> Angstrom.string ": " *> take_while1 Char.is_alpha <* wss)

  let file_parser = many line_parser

  let run contents = parse_string ~consume:Consume.Prefix file_parser contents
end

let is_valid_password_p1 (p : password_with_conf) =
  let count = p.password |> String.count ~f:(Char.equal p.range.letter) in
  count >= p.range.min && count <= p.range.max

let is_valid_password_p2 (p : password_with_conf) =
  (* NAND *)
  Bool.equal
    (Char.equal p.password.[p.range.min - 1] p.range.letter)
    (Char.equal p.password.[p.range.max - 1] p.range.letter)
  |> not

let () =
  let file_contents =
    filepath |> In_channel.with_file ~f:(fun file -> In_channel.input_all file)
  in
  match Parser.run file_contents with
  | Ok passwords ->
      let valid_psw_count_phase1 =
        passwords |> List.count ~f:is_valid_password_p1
      in
      printf "Phase 1: %i\n" valid_psw_count_phase1;
      let valid_psw_count_phase2 =
        passwords |> List.count ~f:is_valid_password_p2
      in
      printf "Phase 2: %i\n" valid_psw_count_phase2
  | Error error -> printf "Error: %s\n" error
