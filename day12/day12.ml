open Base

type instruction =
  | North of int
  | South of int
  | East of int
  | West of int
  | Left of int
  | Right of int
  | Forward of int
[@@deriving show]

type point2 = { x : int; y : int } [@@deriving show]

let origin = { x = 0; y = 0 }

let distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)

let rotate_point p degrees =
  match degrees with
  | 90 -> { x = -p.y; y = p.x }
  | 180 -> { x = -p.x; y = -p.y }
  | 270 -> { x = p.y; y = -p.x }
  | -90 -> { x = p.y; y = -p.x }
  | -180 -> { x = -p.x; y = -p.y }
  | -270 -> { x = -p.y; y = p.x }
  | _ -> p

let dirs =
  [| { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 }; { x = 0; y = -1 } |]

type state = { pos : point2; waypoint : point2; dir : int } [@@deriving show]

let initial_state =
  { pos = origin; waypoint = { x = 10; y = 1 }; dir = 0 }
  [@@deriving show]

let parse_instruction line =
  let instruction = line.[0] in
  let num =
    line |> String.sub ~pos:1 ~len:(String.length line - 1) |> Int.of_string
  in
  match (instruction, num) with
  | 'N', n -> Ok (North n)
  | 'S', n -> Ok (South n)
  | 'E', n -> Ok (East n)
  | 'W', n -> Ok (West n)
  | 'L', n -> Ok (Left n)
  | 'R', n -> Ok (Right n)
  | 'F', n -> Ok (Forward n)
  | _ -> Error "Invalid character in input file"

let step_p1 s instruction =
  match instruction with
  | North n -> { s with pos = { x = s.pos.x; y = s.pos.y + n } }
  | East n -> { s with pos = { x = s.pos.x + n; y = s.pos.y } }
  | South n -> { s with pos = { x = s.pos.x; y = s.pos.y - n } }
  | West n -> { s with pos = { x = s.pos.x - n; y = s.pos.y } }
  | Left n -> { s with dir = (s.dir + (n / 90)) % 4 }
  | Right n -> { s with dir = (s.dir - (n / 90)) % 4 }
  | Forward n ->
      let dir = dirs.(s.dir) in
      { s with pos = { x = s.pos.x + (dir.x * n); y = s.pos.y + (dir.y * n) } }

let step_p2 s instruction =
  match instruction with
  | North n -> { s with waypoint = { x = s.waypoint.x; y = s.waypoint.y + n } }
  | East n -> { s with waypoint = { x = s.waypoint.x + n; y = s.waypoint.y } }
  | South n -> { s with waypoint = { x = s.waypoint.x; y = s.waypoint.y - n } }
  | West n -> { s with waypoint = { x = s.waypoint.x - n; y = s.waypoint.y } }
  | Left n -> { s with waypoint = rotate_point s.waypoint n }
  | Right n -> { s with waypoint = rotate_point s.waypoint (-n) }
  | Forward n ->
      {
        s with
        pos =
          { x = s.pos.x + (s.waypoint.x * n); y = s.pos.y + (s.waypoint.y * n) };
      }

let run filepath =
  let open Utils.Result_syntax in
  let* lines = Utils.safe_read_lines filepath in
  let* instructions = List.map lines ~f:parse_instruction |> Result.all in

  let final_state_p1 = List.fold instructions ~init:initial_state ~f:step_p1 in
  let part1 = distance origin final_state_p1.pos in

  let final_state_p2 = List.fold instructions ~init:initial_state ~f:step_p2 in
  let part2 = distance origin final_state_p2.pos in

  Ok (part1, part2)
