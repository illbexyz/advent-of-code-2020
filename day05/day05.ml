open Base
open Stdio

let id x = x

module Seat = struct
  type t = string * string

  let of_string code : t =
    let split_idx =
      String.fold_until code ~init:0
        ~f:
          (fun idx -> function
            | 'F' | 'B' -> Continue (idx + 1)
            | 'L' | 'R' -> Stop idx
            | _ -> failwith "unexpected char")
        ~finish:id
    in
    let len = String.length code in
    (String.prefix code split_idx, String.suffix code (len - split_idx))

  let binary_to_id code =
    let len = String.length code in
    String.foldi code ~init:0 ~f:(fun idx acc -> function
      | 'F' | 'L' -> acc
      | 'B' | 'R' -> (2 ** (len - idx - 1)) + acc
      | _ -> failwith "unexpected char")

  let get_id ((row, col) : t) = (binary_to_id row * 8) + binary_to_id col
end

let run filepath =
  let seats = In_channel.read_lines filepath in
  let ids = seats |> List.map ~f:Seat.of_string |> List.map ~f:Seat.get_id in
  let sorted_ids = List.sort ids ~compare:Int.compare in
  let min_id = List.hd_exn sorted_ids in
  let my_id =
    ids
    |> List.sort ~compare:Int.compare
    |> List.fold_until ~init:(-1)
         ~f:(fun acc curr ->
           if acc = -1 || curr - 1 = acc then Continue curr else Stop (acc - 1))
         ~finish:id
  in
  Ok (min_id, my_id)
