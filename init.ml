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
  let read = open_in quotes in
  let lines = array_of_lines read in
  close_in read; lines

