open Lwt
open Cohttp_lwt_unix

let (>==) v f =
  match v with
  | None -> None
  | Some(v') -> f v'

type bot = { token : string; debug : bool }

let empty_bot () = { token = ""; debug = false }

let with_debug bot = { bot with debug = true }

type bot_info = {
  bot_id : string;
  bot_first_name : string;
  bot_last_name : string;
  username : string;
  can_join_groups : bool;
  can_read_all_group_messages : bool;
  supports_inline_queries : bool;
  can_connect_to_business : bool;
}

type chat_id = ChatId of string
type user_id = UserId of string
type message_id = MessageId of string
type message_text = MessageText of string

type event =
  | MessageFromUser of chat_id * message_id * user_id * message_text (* user_id *)


let bot_of token = { token; debug = false }

type backend_api_commands =
  | GetMe
  | GetUpdates
  | SendMessage

type low_level_commands =
  | PrintToConsole of string
  | SendMessage of chat_id * message_text (* id * text *)
  | ReplyToMessage of chat_id * message_id * message_text

let bot_ref = ref (empty_bot ())

let get_bot_ref () =
  !bot_ref

let api_url_endpoint_of = function
  | GetMe -> "/getMe"
  | GetUpdates -> "/getUpdates"
  | SendMessage -> "/sendMessage"

let api_url_of { token; _ } cmd =
  "https://api.telegram.org/bot" ^ token ^ (api_url_endpoint_of cmd)

let try_ok j =
  let open Yojson.Basic.Util in
  match j |> member "ok" |> to_bool_option with
  | None -> None
  | Some(_) -> Some (j |> member "result")

let parse_events (events : Yojson.Basic.t list) =
  let open Yojson.Basic.Util in
  let exists f j = member f j <> `Null in
  let parse_single_event event =
    if exists "message" event then
      let msg = member "message" event in
      if exists "text" msg then
        let text = msg |> member "text" |> to_string in
        let chat_id = msg |> member "chat" |> member "id" |> to_int |> string_of_int in
        let mess_id = msg |> member "message_id" |> to_int |> string_of_int in
        let user_id = msg |> member "from" |> member "id" |> to_int |> string_of_int in
        MessageFromUser ((ChatId chat_id), (MessageId mess_id), (UserId user_id), (MessageText text))
      else
        failwith @@ Printf.sprintf "Received this:\n%s\n" (Yojson.Basic.pretty_to_string msg)
    else
      failwith @@ Printf.sprintf "Received this:\n%s\n" (Yojson.Basic.pretty_to_string event)
  in
  print_endline (Yojson.Basic.pretty_to_string (`List events));
  List.map parse_single_event events


module BackendAPI = struct
  let latest_unread_update = ref 0

  let add_param p v u = u ^ "?" ^ p ^ "=" ^ v
  let append_param p v u = u ^ "&" ^ p ^ "=" ^ v

  let get_me bot : bot_info option Lwt.t =
    let url = api_url_of bot GetMe in
    Client.get (Uri.of_string url) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let json = Yojson.Basic.from_string body in
    let open Yojson.Basic.Util in
    try_ok json >== fun res ->
      let default_b v = v |> to_bool_option |> Option.value ~default:false in
      let bot_id = res |> member "id" |> to_int |> string_of_int in
      let bot_first_name = res |> member "first_name" |> to_string in
      let bot_last_name = res |> member "last_name" |> to_string_option |> Option.value ~default:"" in
      let username = res |> member "username" |> to_string in
      let can_join_groups = res |> member "can_join_groups" |> default_b in
      let can_read_all_group_messages = res |> member "can_read_all_group_messages" |> default_b in
      let supports_inline_queries = res |> member "supports_inline_queries" |> default_b in
      let can_connect_to_business = res |> member "can_connect_to_business" |> default_b in
      Some ({ bot_id
            ; bot_first_name
            ; bot_last_name
            ; username
            ; can_join_groups
            ; can_read_all_group_messages
            ; supports_inline_queries
            ; can_connect_to_business
            })

  let send_message bot chat_id msg =
    let url = api_url_of bot SendMessage |> add_param "chat_id" chat_id |> append_param "text" msg in
    Client.post (Uri.of_string url) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    if bot.debug then begin
      Printf.printf "%s\n" body
    end

  let reply_message bot chat_id msg_id msg =
    let data = Yojson.Basic.to_string (
      `Assoc
        ([ ("chat_id", `Int (int_of_string chat_id))
         ; ("text", `String msg)
         ; ("reply_parameters",
           `Assoc
             ([ ("message_id", `Int (int_of_string msg_id)) ]))
        ]))
    in
    let url = api_url_of bot SendMessage in
    (Lwt_io.printl @@ Printf.sprintf "DEBUG: %s" data) >>= fun () ->
    let header =
      let open Cohttp.Header in
      let d = init () in
      add d "Content-Type" "application/json"
    in
    Client.post
      ~body:(Cohttp_lwt.Body.of_string data)
      ~headers:header
      (Uri.of_string url) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    if bot.debug then begin
      Printf.printf "%s\n" body
    end

  let fetch_updates { token; debug } =
    let url = api_url_of { token; debug } GetUpdates |> add_param "offset" (string_of_int !latest_unread_update) in
    Client.get (Uri.of_string url) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let json = Yojson.Basic.from_string body in
    if debug then begin
      Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json)
    end;
    try_ok json >== fun res ->
    let open Yojson.Basic.Util in
    let v = res |> to_list |> parse_events in
    let last_update_id =
      res
      |> to_list
      |> List.map (fun x -> x |> member "update_id" |> to_int)
      |> List.fold_left max (-1)
    in
    latest_unread_update := last_update_id + 1;
    Some v
end


let read_token_from_file () =
  let open BatFile in
  with_file_in "token" BatInnerIO.read_all |> BatString.strip

let run_one_shot bot f =
  Lwt_main.run (f bot)


let execute bot (commands : low_level_commands list) : unit Lwt.t =
  let executor = function
    | SendMessage(ChatId(chat_id), MessageText(msg)) ->
      BackendAPI.send_message bot chat_id msg
    | ReplyToMessage(ChatId(chat_id), MessageId(msg_id), MessageText(msg)) ->
      BackendAPI.reply_message bot chat_id msg_id msg
    | PrintToConsole s ->
      Lwt_io.print s
  in
  Lwt_list.iter_s executor commands

let print_error () =
  failwith "Encountered some error when trying to read the json"

let prnt s = PrintToConsole s
let prntnl s = PrintToConsole (s ^ "\n")

let fetch_updates_and_dispatch f =
  let bot = get_bot_ref () in
  let rec loop () : unit Lwt.t =
    BackendAPI.fetch_updates bot >>= fun json_opt ->
    match json_opt with
    | None -> print_error ()
    | Some res ->
    let commands : low_level_commands list = List.map f res in
    execute bot commands >>= fun () ->
    loop ()
  in
  loop ()

let chat_id_of upd =
  let open Yojson.Basic.Util in
  upd |> member "chat_id" |> to_int |> string_of_int

let chat_id_of_event = function
  | MessageFromUser(chat_id, _, _, _) -> chat_id

let message_id_from id = MessageId id

let message_from s = MessageText s

let mess_id_of_event = function
  | MessageFromUser(_, mess_id, _, _) -> mess_id

let send_message chat_id s =
  SendMessage (chat_id, s)

let reply_to_message chat_id msg_id s =
  ReplyToMessage (chat_id, msg_id, s)

let run_poll_bot bot f =
  bot_ref := bot;
  Lwt_main.run (fetch_updates_and_dispatch f)
