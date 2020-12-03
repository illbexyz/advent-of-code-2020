open Base
open Stdio

let wanted_sum = 2020

let run_phase_1 nums =
  let set = Set.of_list (module Int) nums in
  let seq = nums |> Sequence.of_list in

  seq
  |> Sequence.find ~f:(fun x ->
         Set.exists ~f:(fun curr -> curr = wanted_sum - x) set)
  |> Result.of_option ~error:"Phase 1 failed"
  |> Result.map ~f:(fun x -> (wanted_sum - x) * x)

let run_phase_2 nums =
  let set = Set.of_list (module Int) nums in
  let seq = nums |> Sequence.of_list in

  seq
  |> Sequence.cartesian_product seq
  |> Sequence.find ~f:(fun (x, y) ->
         Set.exists ~f:(fun curr -> curr = wanted_sum - x - y) set)
  |> Result.of_option ~error:"Phase 2 failed"
  |> Result.map ~f:(fun (x, y) -> (wanted_sum - x - y) * x * y)

let run () : (int * int, string) Result.t =
  let lines =
    In_channel.with_file "day01/input.txt" ~f:(fun file ->
        In_channel.input_lines file)
  in

  let nums = lines |> List.map ~f:Int.of_string in

  Utils.Result_syntax.(
    let* res1 = run_phase_1 nums in
    let* res2 = run_phase_2 nums in
    return (res1, res2))
