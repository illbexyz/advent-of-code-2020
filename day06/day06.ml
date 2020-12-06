open Base
open Stdio

let filepath = "day06/input.txt"

module Set = struct
  include Set

  let of_string str = of_list (module Char) (String.to_list str)

  let inter_list comparator sets =
    List.foldi sets ~init:(empty comparator) ~f:(fun idx acc curr ->
        if idx > 0 then inter acc curr else curr)
end

let run () =
  let lines = In_channel.read_lines filepath in

  let sets =
    List.group lines ~break:(fun x _ -> String.equal x "")
    |> List.map ~f:(List.filter ~f:(fun s -> String.equal s "" |> not))
    |> List.map ~f:(List.map ~f:Set.of_string)
  in

  let p1 =
    sets
    |> List.map ~f:(Set.union_list (module Char))
    |> List.map ~f:Set.length |> List.fold ~init:0 ~f:( + )
  in

  let p2 =
    sets
    |> List.map ~f:(Set.inter_list (module Char))
    |> List.map ~f:Set.length |> List.fold ~init:0 ~f:( + )
  in
  Ok (p1, p2)
