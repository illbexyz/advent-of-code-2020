open Base

type instruction = Mask of string | Mem of int * int

module Parser = struct
  open Angstrom

  let wss = take_while Char.is_whitespace

  let num = take_while Char.is_digit

  let mask =
    (fun mask -> Mask mask)
    <$> (wss *> string "mask" *> wss *> char '=' *> wss
         *> take_while Char.is_alphanum
        <* wss)

  let mem =
    (fun address value -> Mem (Int.of_string address, Int.of_string value))
    <$> wss *> (string "mem[" *> num <* char ']')
    <*> wss *> char '=' *> wss *> num
    <* wss

  let instructions = many (mask <|> mem)

  let run contents = parse_string ~consume:Consume.Prefix instructions contents
end

let bits_to_int bits = "0b" ^ bits |> Int.of_string

let int_to_bits padding num =
  let bin =
    match num with
    | 0 -> "0"
    | _ ->
        let s = ref "" in
        let n = ref num in
        while !n > 0 do
          s := (if !n % 2 = 0 then "0" else "1") ^ !s;
          n := !n / 2
        done;
        !s
  in
  let padding = String.make (padding - String.length bin) '0' in
  padding ^ bin

let run_part_1 instructions =
  List.fold instructions
    ~init:(Map.empty (module Int), "")
    ~f:(fun (memory, mask) instruction ->
      match instruction with
      | Mask next_mask -> (memory, next_mask)
      | Mem (addr, value) ->
          let and_mask =
            String.substr_replace_all mask ~pattern:"X" ~with_:"1"
            |> bits_to_int
          in
          let or_mask =
            String.substr_replace_all mask ~pattern:"X" ~with_:"0"
            |> bits_to_int
          in
          let next_value = Int.bit_and value and_mask |> Int.bit_or or_mask in
          let next_memory = Map.set memory ~key:addr ~data:next_value in
          (next_memory, mask))
  |> fst

let run_part_2 instructions =
  List.fold instructions
    ~init:(Map.empty (module Int), "")
    ~f:(fun (memory, mask) instruction ->
      match instruction with
      | Mask next_mask -> (memory, next_mask)
      | Mem (addr, value) ->
          let res_mask =
            List.zip_exn
              (int_to_bits (String.length mask) addr |> String.to_list)
              (String.to_list mask)
            |> List.map ~f:(function
                 | x, '0' -> x
                 | _, '1' -> '1'
                 | _, 'X' -> 'X'
                 | _ -> '0')
            |> String.of_char_list
          in

          let addrs = ref [ res_mask ] in

          let x_count = String.count mask ~f:(Char.equal 'X') in
          for _ = 1 to x_count do
            addrs :=
              List.concat_map !addrs ~f:(fun mask ->
                  [
                    String.substr_replace_first mask ~pattern:"X" ~with_:"0";
                    String.substr_replace_first mask ~pattern:"X" ~with_:"1";
                  ])
          done;

          let next_memory =
            List.fold !addrs ~init:memory ~f:(fun mem addr ->
                Map.set mem ~key:(bits_to_int addr) ~data:value)
          in

          (next_memory, mask))
  |> fst

let run filepath =
  let open Utils.Result_syntax in
  let* contents = Utils.safe_read_all filepath in
  let* instructions = Parser.run contents in

  let memory_part_1 = run_part_1 instructions in
  let part1 = Map.data memory_part_1 |> List.fold ~init:0 ~f:( + ) in

  let memory_part_2 = run_part_2 instructions in
  let part2 = Map.data memory_part_2 |> List.fold ~init:0 ~f:( + ) in

  Ok (part1, part2)
