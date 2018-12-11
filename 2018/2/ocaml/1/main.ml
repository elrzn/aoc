open Core

(** From a string, return a hash table where all keys are characters contained
    in the given string, and the value the number of instances found in it. *)
let table_character_repeats input =
  let table = Hashtbl.create (module Char) in
  let process letter =
    let repeats = Hashtbl.find_or_add table letter ~default:(fun _ -> 0) in
    Hashtbl.set table ~key:letter ~data:(repeats + 1)
  in
  String.iter input ~f:process ;
  table

let increase_by table value search =
  let found n = Hashtbl.exists table ~f:(fun x -> x = n) in
  value + if found search then 1 else 0

let multipliers =
  let reducer acc table =
    let f = increase_by table in
    (f (fst acc) 2, f (snd acc) 3)
  in
  In_channel.read_lines "input.txt"
  |> List.map ~f:table_character_repeats
  |> List.fold ~init:(0, 0) ~f:reducer

let () = print_endline (Int.to_string (fst multipliers * snd multipliers))
