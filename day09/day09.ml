open Base
open Stdio

let list_slice list start finish =
  List.filteri list ~f:(fun idx _ -> idx >= start && idx < finish)

let gen_possible_sums nums =
  List.cartesian_product nums nums
  |> List.filter ~f:(fun (x, y) -> x <> y)
  |> List.map ~f:(fun (x, y) -> x + y)

let phase_1 preamble_size nums =
  List.fold_until nums ~init:0
    ~f:(fun idx n ->
      if idx < preamble_size then Continue (idx + 1)
      else
        let preamble = list_slice nums (idx - preamble_size) idx in
        let possible_sums = gen_possible_sums preamble in
        match List.mem possible_sums n ~equal:Int.equal with
        | true -> Continue (idx + 1)
        | false -> Stop (Ok n))
    ~finish:(fun _ -> Error "Phase 1: Sum not found")

let phase_2 nums total =
  let open Utils.Result_syntax in
  let array = Array.of_list nums in
  let sum_until_overflow from =
    let acc = ref array.(from) in
    let i = ref (from + 1) in
    while !acc < total do
      acc := !acc + array.(!i);
      i := !i + 1
    done;
    if !acc = total then Some (!i - 1) else None
  in
  let* start, end_ =
    List.fold_until
      (List.range 0 (List.length nums))
      ~init:None
      ~f:(fun _ idx ->
        match sum_until_overflow idx with
        | Some end_ -> Stop (Ok (idx, end_))
        | None -> Continue None)
      ~finish:(fun _ -> Error "Phase 2: Sum not found")
  in
  let sorted =
    list_slice nums start (end_ + 1) |> List.sort ~compare:Int.compare
  in
  return (List.hd_exn sorted + List.last_exn sorted)

let run filepath =
  let open Utils.Result_syntax in
  let lines = In_channel.read_lines filepath in
  let inputs = lines |> List.map ~f:Int.of_string in
  let preamble_size =
    if String.is_substring filepath ~substring:"test" then 5 else 25
  in
  let* p1 = phase_1 preamble_size inputs in
  let* p2 = phase_2 inputs p1 in
  Ok (p1, p2)
