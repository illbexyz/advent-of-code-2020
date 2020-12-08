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
    ]

let parse_day (args : string array) =
  match args with [| _; day |] -> Ok day | _ -> Error "Argument day not found"

let run () =
  Utils.Result_syntax.(
    let* day = parse_day (Sys.get_argv ()) in
    let* runner =
      day |> Map.find runners
      |> Result.of_option ~error:"Missing runner in advent_of_code.ml"
    in
    runner ())

let () =
  match run () with
  | Ok (p1, p2) -> printf "Phase 1: %i\nPhase 2: %i\n" p1 p2
  | Error error -> print_endline error
