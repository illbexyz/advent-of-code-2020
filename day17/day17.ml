open Base

module Cell3 = struct
  module T = struct
    type t = int * int * int [@@deriving ord, sexp_of]

    let possible_nbours =
      let open List.Let_syntax in
      List.range (-1) 2 >>= fun z ->
      List.range (-1) 2 >>= fun x ->
      List.range (-1) 2 >>= fun y ->
      match (z, x, y) with 0, 0, 0 -> [] | _ -> return (z, x, y)

    let sum (z1, x1, y1) (z2, x2, y2) = (z1 + z2, x1 + x2, y1 + y2)
  end

  include T
  include Comparator.Make (T)
end

module Cell4 = struct
  module T = struct
    type t = int * int * int * int [@@deriving ord, sexp_of]

    let possible_nbours =
      let open List.Let_syntax in
      List.range (-1) 2 >>= fun w ->
      List.range (-1) 2 >>= fun z ->
      List.range (-1) 2 >>= fun x ->
      List.range (-1) 2 >>= fun y ->
      match (w, z, x, y) with 0, 0, 0, 0 -> [] | _ -> return (w, z, x, y)

    let sum (w1, z1, x1, y1) (w2, z2, x2, y2) =
      (w1 + w2, z1 + z2, x1 + x2, y1 + y2)
  end

  include T
  include Comparator.Make (T)
end

module Common = struct
  let count_active_cells t = Set.length t

  let step neighbors is_active cell_active_neighbors t =
    Set.filter (neighbors t) ~f:(fun cell ->
        match
          (is_active t cell, List.length (cell_active_neighbors t cell))
        with
        | true, 2 | true, 3 | false, 3 -> true
        | _ -> false)

  let is_active t cell = Set.mem t cell
end

module ConwayCubes3 = struct
  type t = (Cell3.t, Cell3.comparator_witness) Set.t

  let of_lines (lines : string list) : t =
    List.foldi lines
      ~init:(Set.empty (module Cell3))
      ~f:(fun x set row ->
        String.foldi row ~init:set ~f:(fun y set' c ->
            if Char.equal c '#' then Set.add set' (0, x, y) else set'))

  let cell_neighbors c = List.map Cell3.possible_nbours ~f:(Cell3.sum c)

  let neighbors t =
    Set.elements t
    |> List.concat_map ~f:cell_neighbors
    |> Set.of_list (module Cell3)

  let cell_active_neighbors t cell =
    cell_neighbors cell |> List.filter ~f:(Common.is_active t)
end

module ConwayCubes4 = struct
  type t = (Cell4.t, Cell4.comparator_witness) Set.t

  let of_lines (lines : string list) : t =
    List.foldi lines
      ~init:(Set.empty (module Cell4))
      ~f:(fun x set row ->
        String.foldi row ~init:set ~f:(fun y set' c ->
            if Char.equal c '#' then Set.add set' (0, 0, x, y) else set'))

  let cell_neighbors cell =
    List.map Cell4.possible_nbours ~f:(Cell4.sum cell)

  let neighbors t =
    Set.elements t
    |> List.concat_map ~f:cell_neighbors
    |> Set.of_list (module Cell4)

  let cell_active_neighbors t cell =
    cell_neighbors cell |> List.filter ~f:(Common.is_active t)
end

let step_p1 t =
  Common.step ConwayCubes3.neighbors Common.is_active
    ConwayCubes3.cell_active_neighbors t

let step_p2 t =
  Common.step ConwayCubes4.neighbors Common.is_active
    ConwayCubes4.cell_active_neighbors t

let run filepath =
  let open Utils.Result_syntax in
  let* lines = Utils.safe_read_lines filepath in

  let p1_cubes : ConwayCubes3.t = ConwayCubes3.of_lines lines in
  let p1_after_six_steps =
    List.range 0 6 |> List.fold ~init:p1_cubes ~f:(fun c _ -> step_p1 c)
  in
  let part1 = Common.count_active_cells p1_after_six_steps in

  let p2_cubes : ConwayCubes4.t = ConwayCubes4.of_lines lines in
  let p2_after_six_steps =
    List.range 0 6 |> List.fold ~init:p2_cubes ~f:(fun c _ -> step_p2 c)
  in
  let part2 = Common.count_active_cells p2_after_six_steps in

  Ok (part1, part2)
