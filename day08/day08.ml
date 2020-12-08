open Base
open Stdio

let filepath = "day08/input.txt"

module Computer = struct
  type instruction = Nop of int | Acc of int | Jmp of int

  type program = instruction array

  type state = { acc : int; ip : int }

  let initial_state = { acc = 0; ip = 0 }

  let inst_to_string = function
    | Nop x -> Printf.sprintf "Nop %i" x
    | Acc x -> Printf.sprintf "Acc %i" x
    | Jmp x -> Printf.sprintf "Jmp %i" x

  let run_instruction state = function
    | Nop _ -> { acc = state.acc; ip = state.ip + 1 }
    | Acc x -> { acc = state.acc + x; ip = state.ip + 1 }
    | Jmp x -> { acc = state.acc; ip = state.ip + x }

  let step program state =
    let curr_instruction = program.(state.ip) in
    run_instruction state curr_instruction

  let find_loop program =
    let visited = Array.map program ~f:(fun _ -> ref false) in
    let rec run state =
      if state.ip = Array.length program then `Terminate state.acc
      else
        match !(visited.(state.ip)) with
        | true -> `Loop state.acc
        | false ->
            visited.(state.ip) := true;
            run (step program state)
    in
    run initial_state

  let change_instruction_at_idx (program : program) idx instruction =
    Array.mapi program ~f:(fun curr_idx i ->
        if curr_idx = idx then instruction else i)

  module Parser = struct
    open Angstrom

    let wss = take_while Char.is_whitespace

    let int =
      (fun sign digits -> Int.of_string (sign ^ digits))
      <$> (string "+" <|> string "-")
      <*> take_while1 Char.is_digit

    let instruction_int str = wss *> string str *> wss *> int <* wss

    let acc = instruction_int "acc" >>| fun x -> Acc x

    let nop = instruction_int "nop" >>| fun x -> Nop x

    let jmp = instruction_int "jmp" >>| fun x -> Jmp x

    let instruction = acc <|> nop <|> jmp

    let program : program t = many instruction >>| List.to_array

    let run input = parse_string ~consume:Consume.Prefix program input
  end
end

let run () =
  let open Utils.Result_syntax in
  let input = In_channel.read_all filepath in
  let* program = Computer.Parser.run input in

  let* phase1 =
    match Computer.find_loop program with
    | `Loop acc -> Ok acc
    | `Terminate _ -> Error "Phase 1: Loop not found"
  in

  let changed_programs =
    Array.filter_mapi program ~f:(fun idx instruction ->
        match instruction with
        | Acc _ -> None
        | Jmp x -> Some (Computer.change_instruction_at_idx program idx (Nop x))
        | Nop x -> Some (Computer.change_instruction_at_idx program idx (Jmp x)))
  in

  let* phase2 =
    Array.fold_until changed_programs ~init:None
      ~f:(fun _ program ->
        match Computer.find_loop program with
        | `Terminate acc -> Stop (Ok acc)
        | `Loop _ -> Continue None)
      ~finish:(Result.of_option ~error:"Phase 2: Every program ran into a loop")
  in
  Ok (phase1, phase2)
