open Core

let result =
  let table_freq = Hashtbl.create (module Int) in
  In_channel.read_lines "input.txt"
  |> List.map ~f:int_of_string
  |> List.fold_until
    ~init:0
    ~finish:(fun x -> x)
    ~f:(fun acc x ->
        let next = acc + x in
        let found = Hashtbl.find table_freq next in
        match found with
        | Some _ -> Stop next
        | None -> Hashtbl.set table_freq ~key:next ~data:true
                ; Continue next)

let _ =
  print_endline (string_of_int result)
