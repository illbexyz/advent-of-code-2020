open Base
open Stdio

let runners =
  Map.of_alist_exn
    (module String)
    [
      ("day01", Day01.run);
      ("day02", Day02.run);
      ("day03", Day03.run);
      ("day04", Day04.run);
      ("day05", Day05.run);
      ("day06", Day06.run);
      ("day07", Day07.run);
      ("day08", Day08.run);
      ("day09", Day09.run);
      ("day10", Day10.run);
      ("day11", Day11.run);
      ("day12", Day12.run);
      ("day15", Day15.run);
    ]

let parse_args (args : string array) =
  match Array.to_list args with
  | _ :: day :: files -> Ok (day, files)
  | _ -> Error "Argument day not found"

let run_with_file day file =
  Utils.Result_syntax.(
    let* runner =
      day |> Map.find runners
      |> Result.of_option ~error:"Missing runner in advent_of_code.ml"
    in
    runner file)

let runs () =
  let open Utils.Result_syntax in
  let* day, files = parse_args (Sys.get_argv ()) in
  List.map files ~f:(fun file ->
      let* res_p1, res_p2 = run_with_file day file in
      return
        (Printf.sprintf "%s\nPhase 1: %i\nPhase 2: %i\n" file res_p1 res_p2))
  |> Result.all

let () =
  match runs () with
  | Ok boh -> List.iter boh ~f:(fun res -> print_endline res)
  | Error error -> prerr_endline error
