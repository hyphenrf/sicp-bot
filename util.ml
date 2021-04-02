let open_append =
    open_out_gen [Open_creat; Open_append; Open_nonblock] 0o660

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
      arr.(i) <- Queue.pop lines
    done;
  arr

let request ?data ?auth ~host path =
  let url = "https://"^host^path in
  let (meth, body) = match data with
    | None    -> (`GET, "")
    | Some xs -> (`POST, "{"^
        (List.map (fun (a,b) -> Printf.sprintf {|"%s":"%s"|} a b) xs
          |> String.concat ",")
      ^"}")
  in
  let headers = ("Content-type", "application/json")::
    match auth with
    | None     -> []
    | Some tok -> ["Authorization", tok]
  in
  let request = let open Curly in
    Request.make ~meth ~url ~headers ~body ()
  in
  match Curly.run request with
  | Ok response -> response
  | Error error ->
      let buf = Buffer.create (16*1024) in
      let ppf = Format.formatter_of_buffer buf in
      Format.fprintf ppf "Curly Error: %a" Curly.Error.pp error;
      failwith (Buffer.contents buf)

let handle ~ok ~err (resp: Curly.Response.t) = 
  if resp.code = 200
  then ok  resp
  else err resp
