open Base
open Stdio

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

let run_phase_1 passwords =
  let res = passwords |> List.count ~f:is_valid_password_p1 in
  Ok res

let run_phase_2 passwords =
  let res = passwords |> List.count ~f:is_valid_password_p2 in
  Ok res

let run filepath =
  let file_contents =
    filepath |> In_channel.with_file ~f:(fun file -> In_channel.input_all file)
  in
  Utils.Result_syntax.(
    let* passwords = Parser.run file_contents in
    let* res1 = run_phase_1 passwords in
    let* res2 = run_phase_2 passwords in
    return (res1, res2))
