open Base
open Stdio

let list_slice arr start finish =
  assert (List.length arr >= start);
  assert (List.length arr <= finish);
  List.sub arr ~pos:start ~len:(finish - start)

module AdaptersGraph = struct
  type node = int

  type t = (node, node list, Int.comparator_witness) Map.t

  let from_sorted_list adapters : t =
    List.mapi adapters ~f:(fun i a ->
        let slice = list_slice adapters (i + 1) (List.length adapters) in
        let edges = List.take_while slice ~f:(fun j -> j <= a + 3) in
        (a, edges))
    |> Map.of_alist_exn (module Int)

  let get_neighbours (graph : t) node = Map.find_exn graph node

  let node_count (graph : t) = Map.length graph

  let node_list (graph : t) = Map.keys graph

  let visit graph from to_find =
    let rec dfs n history =
      if n = to_find && List.length history = node_count graph - 1 then
        Some (n :: history)
      else
        List.fold_until (get_neighbours graph n) ~init:None
          ~f:(fun _ m ->
            match dfs m (n :: history) with
            | Some path -> Stop (Some path)
            | None -> Continue None)
          ~finish:(fun _ -> None)
    in
    dfs from [] |> Option.map ~f:List.rev

  let possible_paths graph =
    let nodes = node_list graph |> List.sort ~compare:Int.compare in
    (* The score of a node is the number of the possible paths going
       from that node to the last one *)
    let scores = Hashtbl.create (module Int) in

    List.iter nodes ~f:(fun n ->
        let n_score = Hashtbl.find scores n |> Option.value ~default:1 in
        get_neighbours graph n
        |> List.iter ~f:(fun m ->
               let m_curr_score =
                 Hashtbl.find scores m |> Option.value ~default:0
               in
               let m_next_score = n_score + m_curr_score in
               Hashtbl.set scores ~key:m ~data:m_next_score));

    Hashtbl.find_exn scores (List.last_exn nodes)
end

let count_differences path =
  let one, two, three = (ref 0, ref 0, ref 0) in
  let array = Array.of_list path in
  try
    for i = 1 to List.length path - 1 do
      match (array.(i - 1), array.(i)) with
      | x, y when x = y - 1 -> one := !one + 1
      | x, y when x = y - 2 -> two := !two + 1
      | x, y when x = y - 3 -> three := !three + 1
      | x, y ->
          let msg =
            Printf.sprintf
              "path contains two subsequent numbers of difference > 3: %i and \
               %i"
              x y
          in
          raise (Invalid_argument msg)
    done;
    Ok (!one, !two, !three)
  with Invalid_argument error -> Error error

let run filepath =
  let open Utils.Result_syntax in
  let adapters =
    In_channel.read_lines filepath
    |> List.map ~f:Int.of_string
    |> List.sort ~compare:Int.compare
  in
  let device_adapter = List.last_exn adapters |> ( + ) 3 in
  let adapters' = List.append (0 :: adapters) [ device_adapter ] in
  let graph = AdaptersGraph.from_sorted_list adapters' in

  let source = List.hd_exn adapters' in
  let dest = List.last_exn adapters' in
  let* path =
    AdaptersGraph.visit graph source dest
    |> Result.of_option ~error:"Phase 1: Can't find a path"
  in

  let* one_diff, _, three_diff = count_differences path in
  let p1 = one_diff * three_diff in

  let p2 = AdaptersGraph.possible_paths graph in

  Ok (p1, p2)
