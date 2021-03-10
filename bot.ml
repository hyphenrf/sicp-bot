(* Daily SICP Quotes *)
(*-------------------*)

open Util
open Init

module DB = Set.Make(Digest)

let past = fun () ->
  if not @@ Sys.file_exists quoted then DB.empty
  else
    let chan = open_in quoted in
    let rec fill db =
      match input_line chan |> String.trim with
      | exception End_of_file -> db
      | line -> fill (DB.add (Digest.from_hex line) db)
    in
    let db = fill DB.empty in
    close_in chan; db
    
let commit db past =
  let chan = open_append quoted in
  let diff = DB.diff db past in
  DB.iter (fun hash ->
    output_string chan @@ Digest.to_hex hash; output_char chan '\n') diff;
  close_out chan

let send s = print_endline s (*
request ~host
  "/api/v1/statuses"
  ~auth
  ~data:[ "status", s
        ; "visibility", "public" ]
  |> handle ~ok:(fun _-> `succ)
            ~err:(fun resp ->
              Printf.eprintf "Error: %d %s" resp.code resp.body; `fail)
              *)

let post quote db =
  let hash = Digest.string quote in
  let past = past () in
  if DB.mem hash db then begin
     let len = DB.fold (fun _ x -> x + 1) past 0 in
     if len = Array.length lines then
        `fail, db
     else
        `dupe, db
  end
  else begin
     let db = DB.add hash db in
     (* TODO: POST request here *)
        send quote |> function
        | `succ -> commit db past; `succ, db
        | `fail -> `fail, db
  end

(*
let shuf arr = if Array.length arr > 2 then
  for _ = 1 to Array.length arr - 1 do
  	let x = Random.int (Array.length arr - 1) in
  	let top = arr.!(0) in
  	  arr.!(0)<- arr.!(x);
  	  arr.!(x)<- top
  done

let secs = 86400
let _daily = fun () ->
  shuf lines;
  let rec loop i db =
    if i > 0 then
       match post lines.!(i-1) db with
       | `succ, newdb -> Unix.sleep secs; loop (i-1) newdb
       | `dupe, db    -> loop (i-1) db
       | `fail, db    -> loop i db
  in loop (Array.length lines) (past ())
*)

let _once = fun () ->
  let rec attempt () =
  let index = Random.int (Array.length lines) in
    match post lines.!(index) (past ()) with
    | `fail, _ -> exit 1
    | `dupe, _ -> attempt ()
    | `succ, _ -> ()
  in attempt ()

let _main = _once ()
