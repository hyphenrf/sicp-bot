open Util

let init =
  Random.self_init ()

let quotes  = "quotes"
let quoted  = "db.obj"
let host    = "anime.website"
let name    = "OCaml backend"
let scope   = "write:statuses"

let auth = let module M = Serial.F (struct 
  let host  = host
  let name  = name
  let scope = scope
end) in M.auth

let lines =
  match
    List.(Sites.Sites.stuff
          |> filter_map
              (fun d -> Filename.concat d quotes
               |> fun f -> if Sys.file_exists f then Some f else None)
          |> hd)
  with
     | exception Failure _ -> failwith "Quotes file doesn't exist"
     | quotes ->
  let read = open_in quotes in
  let lines = array_of_lines read in
  close_in read; lines

