
(* Daily SICP Quotes *)
(* @sick_p_bot *)
(*-------------------*)

(* Configs: *)
let secs = 86400
let quotes = "quotes"
let quoted = "db.obj"
(*-----------------*)

module DB = Set.Make(Digest)

let open_append =
    open_out_gen [Open_creat; Open_append; Open_nonblock] 0o660
let (.!()<-) = Array.unsafe_set
let (.!())   = Array.unsafe_get

let array_of_lines chan =
  let lines = Queue.create () in
    begin try
      while true do
        Queue.push (input_line chan) lines
      done
    with End_of_file -> ()
    end;

    let arr = Array.make (Queue.length lines) "" in
    for i = 0 to Array.length arr - 1 do
      arr.!(i)<- Queue.pop lines
    done;
  arr

let () = Random.self_init ()
let shuf arr = if Array.length arr > 2 then
  for i = 1 to Array.length arr - 1 do
  	let x = Random.int (Array.length arr - 1) in
  	let top = arr.!(0) in
  	  arr.!(0)<- arr.!(x);
  	  arr.!(x)<- top
  done
  

let lines =
  let read = open_in quotes in
  let lines = array_of_lines read in
  close_in read; lines
  
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
        print_endline quote;
        commit db past;
     (* TODO -------------------*)
     `succ, db
  end

let _daily = fun () ->
  shuf lines;
  let rec loop i db =
    if i > 0 then
       match post lines.!(i-1) db with
       | `succ, newdb -> Unix.sleep secs; loop (i-1) newdb
       | `dupe, db    -> loop (i-1) db
       | `fail, db    -> loop i db
  in loop (Array.length lines) (past ())

let _lambda = fun () ->
  let rec attempt () =
  let index = Random.int (Array.length lines) in
    match post lines.!(index) (past ()) with
    | `fail, _ -> print_endline "Fail"; exit 1
    | `dupe, _ -> print_endline "Dupe"; attempt ()
    | `succ, _ -> print_endline "Succ"; ()
  in attempt ()

let _main = _lambda ()
