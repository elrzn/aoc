open Core

let () =
  In_channel.read_lines "input.txt"
  |> List.map ~f:int_of_string
  |> List.fold ~init:0 ~f:(+)
  |> string_of_int
  |> print_endline
