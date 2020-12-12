open Base

module Grid = struct
  type cell = Empty | Occupied | Floor

  type part = PartOne | PartTwo

  type t = cell array array

  let from_lines lines : (t, string) Result.t =
    let exception Invalid_char in
    try
      Array.of_list lines
      |> Array.map ~f:(fun s ->
             String.to_array s
             |> Array.map ~f:(function
                  | 'L' -> Empty
                  | '.' -> Floor
                  | '#' -> Occupied
                  | _ -> raise @@ Invalid_char))
      |> fun x -> Ok x
    with Invalid_char -> Error "Invalid char"

  let safe_get grid (x, y) = try Some grid.(x).(y) with _ -> None

  let sight grid (x, y) (dir_x, dir_y) =
    let curr_pos = ref (x + dir_x, y + dir_y) in
    let curr_cell = ref (safe_get grid !curr_pos) in
    while match !curr_cell with Some Floor -> true | _ -> false do
      curr_pos := (fst !curr_pos + dir_x, snd !curr_pos + dir_y);
      curr_cell := safe_get grid !curr_pos
    done;
    !curr_cell

  let directions =
    let dirs = [ -1; 0; 1 ] in
    List.cartesian_product dirs dirs
    |> List.filter ~f:(function 0, 0 -> false | _ -> true)

  let adjacents grid (x, y) =
    directions
    |> List.map ~f:(fun (x_dir, y_dir) -> safe_get grid (x + x_dir, y + y_dir))
    |> List.filter_opt

  let adjacents_sight grid (x, y) =
    directions |> List.map ~f:(sight grid (x, y)) |> List.filter_opt

  let mapi (grid : t) ~f : t =
    Array.mapi grid ~f:(fun x _ ->
        Array.mapi grid.(x) ~f:(fun y curr -> f (x, y) curr))

  let step grid part =
    let get_adjacents, occupied_threshold =
      match part with
      | PartOne -> (adjacents, 4)
      | PartTwo -> (adjacents_sight, 5)
    in
    let has_changed = ref false in
    let next_grid =
      mapi grid ~f:(fun (x, y) curr ->
          let n_occupied =
            get_adjacents grid (x, y)
            |> List.filter ~f:(function Occupied -> true | _ -> false)
            |> List.length
          in
          match (curr, n_occupied) with
          | Empty, 0 ->
              has_changed := true;
              Occupied
          | Occupied, x when x >= occupied_threshold ->
              has_changed := true;
              Empty
          | _ -> curr)
    in
    (next_grid, !has_changed)

  let run grid part =
    let curr_grid, has_changed = (ref grid, ref true) in
    while !has_changed do
      let next_grid, diff = step !curr_grid part in
      curr_grid := next_grid;
      has_changed := diff
    done;
    !curr_grid

  let count_occupied (grid : t) =
    Array.to_list grid |> Array.concat
    |> Array.filter ~f:(function Occupied -> true | _ -> false)
    |> Array.length
end

let run filepath =
  let open Utils.Result_syntax in
  let* lines = Utils.safe_read_lines filepath in
  let* grid = Grid.from_lines lines in

  let p1 = Grid.run grid Grid.PartOne |> Grid.count_occupied in
  let p2 = Grid.run grid Grid.PartTwo |> Grid.count_occupied in

  Ok (p1, p2)
