module F (M:sig val host : string
                val name : string
                val scope: string end)
= struct

let host, name, scope = M.(host, name, scope)
let request = Util.request

let bind f = function
  | Ok x -> f x
  | Error e -> Error e

let ( |>* ) m f = bind f m


type register = {
	redirect_uri  : string;
	client_id     : string;
	client_secret : string;
	vapid_key     : string;
} [@@deriving of_yojson {strict=false}]

let register name = request ~host
  "/api/v1/apps"
  ~data:[ "client_name",   name
        ; "redirect_uris", "urn:ietf:wg:oauth:2.0:oob"
        ; "scopes",        scope ]
  |> Util.handle
       ~ok:(fun resp -> resp.body) 
       ~err:(fun resp -> failwith
         (Printf.sprintf "Register: %d\n%s" resp.code resp.body))
  |> Yojson.Safe.from_string
  |> register_of_yojson


let authorize req = begin
  "https://"^host^"/oauth/authorize?"^String.concat "&" [
    "response_type=code";
    "client_id="^req.client_id;
    "redirect_uri="^req.redirect_uri;
    "scopes="^scope;
  ]^"\nLogin and then paste code and press enter." end
  |> print_endline
  |> read_line
  |> String.trim
  |> fun code -> Ok (req, code)


type token = {
	access_token : string;
	token_type   : string;
	scope        : string;
	created_at   : int;
} [@@deriving of_yojson {strict=false}]

let get_token (req, code) = request ~host
  "/oauth/token"
  ~data:[ "client_id",     req.client_id
        ; "client_secret", req.client_secret
        ; "redirect_uri",  req.redirect_uri
        ; "scopes",        scope
        ; "code",          code
        ; "grant_type",    "authorization_code" ]
  |> Util.handle
       ~ok:(fun resp -> resp.body) 
       ~err:(fun resp -> failwith
         (Printf.sprintf "Get Token: %d\n%s" resp.code resp.body))
  |> Yojson.Safe.from_string
  |> token_of_yojson

let auth = match Sys.getenv_opt "TOKEN" with
  | Some tok -> String.trim tok
  | None ->
  begin
      prerr_endline "No token provided. Requesting a new token";
      register name |>* authorize |>* get_token |> function
        | Ok t -> let tok = t.token_type ^" "^ t.access_token in
               Printf.printf "\n\n Please call the bot with TOKEN=%s \
                 environment variable to continue posting with the current \
                 authorization.\n" tok;
               tok
        | Error s -> failwith s
  end

end
