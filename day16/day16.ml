open Base

type rule = string * (int * int) * (int * int)

type ticket = int list

type train = {
  rules : rule list;
  my_ticket : ticket;
  nearby_tickets : ticket list;
}

let string_of_list list =
  List.fold list ~init:"" ~f:(fun acc x -> acc ^ " " ^ x)

module Parser = struct
  open Angstrom

  let wss = take_while Char.is_whitespace

  let label = wss *> take_while1 Char.is_alpha <* wss

  let num = take_while1 Char.is_digit >>| Int.of_string

  let range : (int * int) t =
    (fun x y -> (x, y)) <$> wss *> num <* char '-' <*> num <* wss

  let rule : rule t =
    (fun label range1 range2 -> (label, range1, range2))
    <$> ((fun l ls -> l :: ls |> string_of_list)
        <$> wss *> label <*> many label <* char ':')
    <*> (wss *> range <* wss)
    <*> string "or" *> wss *> range

  let ticket : ticket t =
    (fun x xs -> x :: xs) <$> num <*> many (char ',' *> num) <* wss

  let train : train t =
    (fun rules my_ticket nearby_tickets -> { rules; my_ticket; nearby_tickets })
    <$> many rule
    <*> wss *> string "your ticket:" *> wss *> ticket
    <*> wss *> string "nearby tickets:" *> wss *> many ticket
    <* wss

  let run contents = parse_string ~consume:Consume.Prefix train contents
end

let n_is_valid (_, (x1, x2), (y1, y2)) n =
  (n >= x1 && n <= x2) || (n >= y1 && n <= y2)

let invalid_n_in_ticket rules ticket : int list =
  List.filter ticket ~f:(fun n ->
      List.for_all rules ~f:(fun rule -> not (n_is_valid rule n)))

let is_ticket_valid rules ticket =
  invalid_n_in_ticket rules ticket |> List.length = 0

let columns_from_tickets tickets =
  let n_cols = List.hd_exn tickets |> List.length in
  let arr_tickets = List.map tickets ~f:Array.of_list in
  List.range 0 n_cols
  |> List.map ~f:(fun n -> List.map arr_tickets ~f:(fun ticket -> ticket.(n)))

let valid_fields (rules : rule list) column =
  List.filter rules ~f:(fun rule -> List.for_all column ~f:(n_is_valid rule))
  |> List.map ~f:(fun (label, _, _) -> label)

let run filepath =
  let open Utils.Result_syntax in
  let* contents = Utils.safe_read_all filepath in
  let* train = Parser.run contents in

  let part1 =
    List.concat_map train.nearby_tickets ~f:(invalid_n_in_ticket train.rules)
    |> List.fold ~init:0 ~f:( + )
  in

  let valid_tickets =
    List.filter train.nearby_tickets ~f:(is_ticket_valid train.rules)
    |> fun tickets -> train.my_ticket :: tickets
  in

  let columns = columns_from_tickets valid_tickets in

  let satisfied_rules_by_column =
    List.map columns ~f:(valid_fields train.rules)
  in

  let part2 =
    let set = Hash_set.create (module String) in
    List.mapi satisfied_rules_by_column ~f:(fun idx labels -> (idx, labels))
    |> List.sort ~compare:(fun (_, s1) (_, s2) ->
           Int.compare (List.length s1) (List.length s2))
    |> List.map ~f:(fun (idx, valid_labels) ->
           List.filter valid_labels ~f:(fun label ->
               not (Hash_set.mem set label))
           |> List.hd_exn
           |> fun label ->
           Hash_set.add set label;
           (label, idx))
    |> List.filter ~f:(fun (label, _) ->
           String.is_substring label ~substring:"departure")
    |> List.map ~f:(fun (_, col) -> List.nth_exn train.my_ticket col)
    |> List.fold ~init:1 ~f:( * )
  in

  Ok (part1, part2)
