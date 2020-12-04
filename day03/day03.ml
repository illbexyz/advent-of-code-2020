open Base
open Stdio

let filepath = "day03/input.txt"

let slopes = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]

module Forest = struct
  type cell = Open | Tree

  type t = cell array array

  let build lines : t =
    lines |> Array.of_list
    |> Array.map ~f:(fun line ->
           line |> String.to_array
           |> Array.map ~f:(fun c -> if Char.equal c '.' then Open else Tree))

  let rows (forest : t) = Array.length forest

  let cols (forest : t) = Array.length forest.(0)

  let get_cell (x, y) (forest : t) =
    let max_y = forest.(0) |> Array.length in
    let mod_y = Int.rem y max_y in
    match x < rows forest with true -> Some forest.(x).(mod_y) | false -> None

  let string_of_cell = function Open -> "Open" | Tree -> "Tree"
end

let run () =
  let lines = In_channel.read_lines filepath in
  let forest = Forest.build lines in
  let start_pos = (0, 0) in

  let rec descend (curr_x, curr_y) (right, down) trees_count =
    let next_pos = (curr_x + down, curr_y + right) in
    match Forest.get_cell next_pos forest with
    | Some Tree -> descend next_pos (right, down) (trees_count + 1)
    | Some Open -> descend next_pos (right, down) trees_count
    | None -> trees_count
  in

  let phase_1_res = descend start_pos (3, 1) 0 in
  let phase_2_res =
    let tree_counts =
      slopes |> List.map ~f:(fun slope -> descend start_pos slope 0)
    in
    List.fold tree_counts ~init:1 ~f:(fun x y -> x * y)
  in
  Ok (phase_1_res, phase_2_res)
