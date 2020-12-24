open Base

module Expression = struct
  type expression =
    | Add of expression * expression
    | Mult of expression * expression
    | Num of int

  let add e1 e2 = Add (e1, e2)

  let mult e1 e2 = Mult (e1, e2)

  let rec eval e =
    match e with
    | Num n -> n
    | Add (e1, e2) -> eval e1 + eval e2
    | Mult (e1, e2) -> eval e1 * eval e2

  module Parser = struct
    open Angstrom

    let chainl1 e op =
      let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
      e >>= fun init -> go init

    let num : expression t =
      take_while1 Char.is_digit >>| Int.of_string >>| fun n -> Num n

    let add_op = string " + " *> return add

    let mult_op = string " * " *> return mult

    let expression_p1 : expression t =
      fix (fun expression ->
          let parens = char '(' *> expression <* char ')' in
          let base_term = parens <|> num in
          let term = chainl1 base_term (add_op <|> mult_op) in
          term)

    let expression_p2 : expression t =
      fix (fun expression ->
          let parens = char '(' *> expression <* char ')' in
          let base_term = parens <|> num in
          let sums = chainl1 base_term add_op in
          let prods = chainl1 sums mult_op in
          prods)

    let run_p1 line = parse_string ~consume:Consume.All expression_p1 line

    let run_p2 line = parse_string ~consume:Consume.All expression_p2 line
  end
end

let run filepath =
  let open Utils.Result_syntax in
  let* lines = Utils.safe_read_lines filepath in

  let* expressions_p1 =
    List.map lines ~f:(fun line -> Expression.Parser.run_p1 line) |> Result.all
  in
  let evals_p1 = List.map expressions_p1 ~f:Expression.eval in
  let part1 = List.fold evals_p1 ~init:0 ~f:( + ) in

  let* expressions_p2 =
    List.map lines ~f:(fun line -> Expression.Parser.run_p2 line) |> Result.all
  in
  let evals_p2 = List.map expressions_p2 ~f:Expression.eval in
  let part2 = List.fold evals_p2 ~init:0 ~f:( + ) in

  Ok (part1, part2)
