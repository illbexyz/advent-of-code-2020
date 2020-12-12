open Base
open Stdio

module Option_syntax = struct
  let ( let* ) x f = Option.bind ~f x

  let return = Option.return
end

module Result_syntax = struct
  let ( let* ) x f = Result.bind ~f x

  let return = Result.return
end

let print_list list to_string =
  List.iter list ~f:(fun x -> x |> to_string |> print_endline)

let safe_read_lines filepath =
  try Ok (In_channel.read_lines filepath) with _ -> Error "File not found"

let safe_read_all filepath =
  try Ok (In_channel.read_all filepath) with _ -> Error "File not found"
