open Core

let explode (str : string) : char list =
  let rec loop acc index =
    if index < 0 then acc else loop (str.[index] :: acc) (index - 1)
  in
  loop [] (String.length str - 1)

let deltas (x : string) (y : string) : int =
  let l1 = explode x in
  let l2 = explode y in
  let matches =
    match List.zip l1 l2 with
    | None -> []
    | Some t -> List.map t ~f:(fun x -> if fst x = snd x then 0 else 1)
  in
  List.fold matches ~init:0 ~f:( + )

let nyi =
  let file = "input.txt" in
  let words = In_channel.read_lines file in
  List.map words ~f:(fun x ->
      List.map words ~f:(fun y ->
          (* TODO Early return? *)
          match deltas x y with 1 -> Some x | _ -> None ) )
