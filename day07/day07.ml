open Base
open Stdio

let filepath = "day07/input.txt"

module Bag = struct
  type t = { name : string; children : (int * string) list } [@@deriving show]

  let build name children : t = { name; children }

  let rec find_bag map (to_find : string) = function
    | { name; _ } when String.equal name to_find -> true
    | { children = []; _ } -> false
    | { children; _ } ->
        List.exists children ~f:(fun (_, name) ->
            find_bag map to_find (Map.find_exn map name))

  let count_inner_bags map bag =
    let rec inner = function
      | { children = []; _ } -> 1
      | { children; _ } ->
          let bag_of_child (quantity, name) =
            quantity * inner (Map.find_exn map name)
          in
          List.map children ~f:bag_of_child |> List.fold ~init:0 ~f:( + )
          |> fun x -> x + 1
    in
    inner bag - 1

  let to_map bags =
    List.map bags ~f:(fun bag -> (bag.name, bag))
    |> Map.of_alist_exn (module String)
end

module Parser = struct
  open Angstrom

  let wss = take_while Char.is_whitespace

  let word = wss *> take_while1 Char.is_alpha <* wss

  let quantity = wss *> (Int.of_string <$> take_while1 Char.is_digit) <* wss

  let bag_name =
    (fun x y -> x ^ " " ^ y)
    <$> word <*> word
    <* choice [ string "bags"; string "bag" ]

  let contained_bag =
    (fun quantity name -> (quantity, name))
    <$> quantity <*> bag_name
    <* choice [ char ','; char '.' ]

  let no_content = (fun _ -> []) <$> string "no other bags."

  let bag =
    (fun name content : Bag.t -> { name; children = content })
    <$> bag_name <* string " contain "
    <*> choice [ no_content; many contained_bag ]

  let bags = many bag

  let run contents = parse_string ~consume:Consume.Prefix bags contents
end

let run () =
  let open Utils.Result_syntax in
  let input = In_channel.read_all filepath in
  let* bags = Parser.run input in
  let bags_map = Bag.to_map bags in
  let p1 =
    List.map bags ~f:(Bag.find_bag bags_map "shiny gold")
    |> List.count ~f:(Bool.equal true)
    |> fun x -> x - 1
  in
  let p2 = Bag.count_inner_bags bags_map (Map.find_exn bags_map "shiny gold") in
  return (p1, p2)
