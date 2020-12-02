open Base

module Option_syntax = struct
  let ( let* ) x f = Option.bind ~f x

  let return = Option.return
end

module Result_syntax = struct
  let ( let* ) x f = Result.bind ~f x

  let return = Result.return
end
