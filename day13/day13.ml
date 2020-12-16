open Base

let run filepath =
  let open Utils.Result_syntax in
  let* lines = Utils.safe_read_lines filepath in
  let* time, buses =
    match lines with
    | [ t; b ] ->
        let t_int = Int.of_string t in
        let bs = String.split b ~on:',' in
        Ok (t_int, bs)
    | _ -> Error "Incorrect input"
  in

  let* bus_id, waiting_time =
    List.map buses ~f:(function
      | "x" -> (-1, Int.max_value)
      | n -> n |> Int.of_string |> fun n -> (n, n - (time % n)))
    |> List.min_elt ~compare:(fun (_, x) (_, y) -> Int.compare x y)
    |> Result.of_option ~error:"Can't find a minimum"
  in

  let part1 = bus_id * waiting_time in

  let constraints =
    List.mapi buses ~f:(fun idx b -> (idx, b))
    |> List.filter ~f:(function _, "x" -> false | _ -> true)
    |> List.map ~f:(fun (idx, b) -> (idx, Int.of_string b))
    |> Array.of_list
  in

  let lcm = ref 1 in
  let time = ref 0 in

  for i = 1 to Array.length constraints - 1 do
    let _, prev_bus_id = constraints.(i - 1) in
    let c_idx, bus_id = constraints.(i) in

    lcm := !lcm * prev_bus_id;

    while (!time + c_idx) % bus_id <> 0 do
      time := !time + !lcm
    done
  done;

  let part2 = !time in

  Ok (part1, part2)
