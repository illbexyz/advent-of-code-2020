open Base

let play nums to_turn =
  let m = Hashtbl.create (module Int) in

  List.iteri nums ~f:(fun idx n -> Hashtbl.set m ~key:n ~data:[ idx + 1 ]);

  let last = ref (List.last_exn nums) in

  for i = List.length nums + 1 to to_turn do
    let prev_occs = Hashtbl.find m !last in
    (last := match prev_occs with Some (x :: y :: _) -> x - y | _ -> 0);
    Hashtbl.set m ~key:!last
      ~data:(i :: Option.value (Hashtbl.find m !last) ~default:[])
  done;

  !last

let run filepath =
  let open Utils.Result_syntax in
  let* contents = Utils.safe_read_all filepath in
  let nums = String.split ~on:',' contents |> List.map ~f:Int.of_string in

  let part1 = play nums 2020 in
  let part2 = play nums 30000000 in

  Ok (part1, part2)
