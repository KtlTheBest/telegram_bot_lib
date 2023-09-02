open Sexplib.Std
open Async
open Cohttp_async

let timeout_default = 30

type bot_actions =
  | NoAction
  | SendMessage of string * string
  | SendFormattedMessage of string * string * format_style
  | ReplyToMessage of string * int * string
  | ReplyToMessageFormatted of string * int * string * format_style

and format_style =
  | Html
  | MarkDown
  | MarkDownV2

let nop = NoAction

let send_message chat_id text = SendMessage(chat_id, text)
let send_formatted_message chat_id text format = SendFormattedMessage(chat_id, text, format)
let send_markdownv2_message chat_id text = send_formatted_message chat_id text MarkDownV2
let send_markdown_message chat_id text = send_formatted_message chat_id text MarkDown
let send_html_message chat_id text = send_formatted_message chat_id text Html

let reply_to_message chat_id msg_id text = ReplyToMessage(chat_id, msg_id, text)
let reply_to_message_formatted chat_id msg_id text format = ReplyToMessageFormatted(chat_id, msg_id, text, format)
let reply_markdownv2 chat_id msg_id text = reply_to_message_formatted chat_id msg_id text MarkDownV2
let reply_markdown chat_id msg_id text = reply_to_message_formatted chat_id msg_id text MarkDown
let reply_html chat_id msg_id text = reply_to_message_formatted chat_id msg_id text Html

let format_option_value = function
  | Html -> "HTML"
  | MarkDown -> "Markdown"
  | MarkDownV2 -> "MarkdownV2"

module type Format_intf = sig
  val bold : string -> string
  val italic : string -> string
  val underscore : string -> string
  val strikethrough : string -> string
  val spoiler : string -> string

  val inline_url : string -> string -> string
  val mention : string -> string -> string
  val emoji : string -> string -> string
  val inline_fixed : string -> string
  val preformatted : string -> string
  val python : string -> string

  val send_message : string -> string -> bot_actions
  val reply_to_message : string -> int -> string -> bot_actions
end

module MarkDownV2 : Format_intf = struct
  let escape_string s =
    let open Str in 
    let star_pat = regexp "\\*" in
    let under_pat = regexp "\\_" in
    s
    |> global_replace star_pat "\\*"
    |> global_replace under_pat "\\_"

  let bold s = "*" ^ escape_string s ^ "*"
  let italic s = "_" ^ escape_string s ^ "_"
  let underscore s = "__" ^ escape_string s ^ "__"
  let strikethrough s = "~" ^ s ^ "~"
  let spoiler s = "||" ^ s ^ "||"
  
  let inline_url caption url = "[" ^ caption ^ "](" ^ url ^ ")"
  let mention caption usr_id = "[" ^ caption ^ "](tg://user?id=" ^ usr_id ^ ")"
  let emoji s e_id = "![" ^ s ^ "](tg://emoji?id=" ^ e_id ^ ")"
  let inline_fixed s = "`" ^ s ^ "`"
  let preformatted s = "```\n" ^ s ^ "```"
  let python s = "```python\n" ^ s ^ "```"

  let send_message = send_markdownv2_message
  let reply_to_message = reply_markdownv2
end

module HTML = struct
  let bold s = "<b>" ^ s ^ "</b>"
  let strong s = "<strong>" ^ s ^ "</strong>"
  let italic s = "<i>" ^ s ^ "</i>"
  let em s = "<em>" ^ s ^ "</em>"
  let underline s = "<u>" ^ s ^ "</u>"
  let underline2 s = "<ins>" ^ s ^ "</ins>"
  let strikethrough s = "<s>" ^ s ^ "</s>"
  let strikethrough2 s = "<strikethrough>" ^ s ^ "</strikethrough>"
  let del s = "<del>" ^ s ^ "</del>"
  let spoiler_span s = "<span class=\"tg-spoiler\">" ^ s ^ "</span>"
  let spoiler s = "<tg-spoiler>" ^ s ^ "</tg-spoiler>"
  let inline_url caption url = "<a href=\"" ^ url ^ "\">" ^ caption ^ "</a>"
  let mention caption usr = "<a href=\"tg://user?id=" ^ usr ^ "\">" ^ caption ^ "</a>"
  let emoji s e_id = "<tg-emoji emoji-id=\"" ^ e_id ^ "\">" ^ s ^ "</tg-emoji>"
  let inline_fixed s = "<code>" ^ s ^ "</code>"
  let preformatted s = "<pre>" ^ s ^ "</pre>"
  let python s = "<pre><code class=\"language-python\">" ^ s ^ "</code></pre>"

  let send_message = send_html_message
  let reply_to_message = reply_html
end

module MarkDown = struct
  let escape_string s =
    let open Str in 
    let star_pat = regexp "\\*" in
    let under_pat = regexp "\\_" in
    s
    |> global_replace star_pat "\\*"
    |> global_replace under_pat "\\_"

  let bold s = "*" ^ escape_string s ^ "*"
  let italic s = "_" ^ escape_string s ^ "_"
  
  let inline_url caption url = "[" ^ caption ^ "](" ^ url ^ ")"
  let mention caption usr_id = "[" ^ caption ^ "](tg://user?id=" ^ usr_id ^ ")"
  let inline_fixed s = "`" ^ s ^ "`"
  let preformatted s = "```\n" ^ s ^ "```"
  let python s = "```python\n" ^ s ^ "```"
  
  let send_message = send_markdown_message
  let reply_to_message = reply_markdown
end

module ReplyKeyboard = struct
  type reply_keyboard_button = { text: string }

  let row s = List.map (fun x -> { text = x }) s
  let keyboard ss = List.map row ss

  let to_yojson keyboard =
    let open Yojson.Basic in
    `Assoc [ "text", keyboard.text ]
end

module InlineKeyboard = struct
  type inline_keyboard_button = 
    { text: string; 
      callback_query: string option 
    }

  let row s = List.map (fun x -> { text = x; callback_query = None }) s
  let keyboard ss = List.map row ss

  let to_yojson keyboard =
    match keyboard.callback_query with
    | None -> 
        let open Yojson.Basic in
        `Assoc [ "text", keyboard.text ]
    | Some s ->
        let open Yojson.Basic in
        `Assoc [ "text", keyboard.text
               ; "callback_query", s
               ]
end

type user_info_basic = {
  user_id: string;
  is_bot: bool;
  first_name: string;
  last_name: string option;
  full_name: string;
  language_code: string option;
  is_premium: bool;
  added_to_attachement_menu: bool
} [@@deriving sexp]

type chat_type = 
  | Private
  | Group
  | SuperGroup
  | Channel
  [@@deriving sexp]

type chat_info_basic = {
  chat_id: string;
  chat_type_val: chat_type;
  title: string option;
  username: string option;
  first_name: string option;
  last_name: string option;
  is_forum: bool
} [@@deriving sexp]

let empty_chat = 
  { chat_id = ""
  ; chat_type_val = Private
  ; title = None
  ; username = None
  ; first_name = None
  ; last_name = None
  ; is_forum = false
  }

let chat_id_of chat = chat.chat_id

type message_contents =
  | Empty
  | TextMessage of string
  [@@deriving sexp]

type message_update_basic = {
  message_id : string;
  message_contents_val: message_contents;
  user: user_info_basic option;
  chat: chat_info_basic option
} [@@deriving sexp]

let message_contents m = m.message_contents_val
let message_id m = int_of_string m.message_id
let chat m = Option.value ~default:empty_chat m.chat
let chat_id m = m |> chat |> chat_id_of

type update_contents =
  | MessageUpdate of message_update_basic
  [@@deriving sexp]

type update_info_basic = {
  update_id: int;
  update_contents: update_contents
} [@@deriving sexp]


let text_message s = TextMessage s
let empty = Empty

type bot_t = Bot of string

let create_bot (tok: string) : bot_t = Bot tok

let stub_bool = Option.value ~default:false

let to_bool_option_stub x = 
  let open Yojson.Basic.Util in
  x |> to_bool_option |> stub_bool

let base_api_url = "https://api.telegram.org/bot"

let (^&^) a b = a ^ "&" ^ b

let get_updates_url offset timeout tok = 
  match offset with
  | Some os -> base_api_url ^ tok ^ "/getUpdates?offset=" ^ os ^&^ "timeout=" ^ string_of_int timeout
  | None -> base_api_url ^ tok ^ "/getUpdates"

let serialize_user json : user_info_basic =
  let open Yojson.Basic.Util in
  let user_id = json |> member "id" |> to_int |> string_of_int in
  let is_bot = json |> member "is_bot" |> to_bool in
  let first_name = json |> member "first_name" |> to_string in
  let last_name = json |> member "last_name" |> to_string_option in
  let language_code = json |> member "language_code" |> to_string_option in
  let is_premium = json |> member "is_premium" |> to_bool_option_stub in
  let added_to_attachement_menu = json |> member "added_to_attachement_menu" |> to_bool_option_stub in
  let full_name = match last_name with
  | Some name -> first_name ^ " " ^ name
  | None -> first_name in
  let (result: user_info_basic) = 
  { user_id = user_id
  ; is_bot = is_bot
  ; first_name = first_name
  ; last_name = last_name
  ; full_name = full_name
  ; language_code = language_code
  ; is_premium = is_premium
  ; added_to_attachement_menu = added_to_attachement_menu
  } in
  result

let resolve_chat_type s =
  match s with
  | "private" -> Private
  | "group" -> Group
  | "supergroup" -> SuperGroup
  | "channel" -> Channel
  | _ -> failwith "Unexpected chat type. Please check the source of the message"

let serialize_chat json : chat_info_basic =
  let open Yojson.Basic.Util in
  let chat_id = json |> member "id" |> to_int |> string_of_int in
  let chat_type_val = json |> member "type" |> to_string |> resolve_chat_type in
  let title = json |> member "title" |> to_string_option in
  let username = json |> member "username" |> to_string_option in
  let first_name = json |> member "first_name" |> to_string_option in
  let last_name = json |> member "last_name" |> to_string_option in
  let is_forum = json |> member "is_forum" |> to_bool_option_stub in
  let result : chat_info_basic =
    { chat_id = chat_id
    ; chat_type_val = chat_type_val
    ; title = title
    ; username = username
    ; first_name = first_name
    ; last_name = last_name
    ; is_forum = is_forum
    } in
    result

let serialize_message_update update : update_contents =
  let open Yojson.Basic.Util in
  let message_id = update |> member "message_id" |> to_int |> string_of_int in
  let sender = update |> member "from" |> serialize_user in
  let chat = update |> member "chat" |> serialize_chat in
  let text = update |> member "text" |> to_string_option in
  let message_contents_val = match text with
    | Some s -> text_message s
    | None -> empty
  in
  let result : message_update_basic = 
    { message_id = message_id
    ; user = Some sender
    ; chat = Some chat
    ; message_contents_val = message_contents_val
    }
  in (MessageUpdate result)

let serialize_json_update update : update_info_basic =
  let open Yojson.Basic.Util in
  let update_id = update |> member "update_id" |> to_int in
  if member "message" update != `Null then
    let update_contents = serialize_message_update (member "message" update) in
    let result : update_info_basic = 
      { update_id = update_id
      ; update_contents = update_contents
      }
    in result
  else
    failwith "TODO: define else branch of serialize_json_update"

let serialize_json raw_text : 'a list =
  let open Yojson.Basic.Util in 
  let data = Yojson.Basic.from_string raw_text in
  let is_ok = data |> member "ok" |> to_bool in
  if is_ok = true then
    data |> member "result" |> to_list
  else
    failwith "Unexpected case: telegram returned false for ok field"

let get_updates offset (tok: string) =
  let url = get_updates_url offset timeout_default tok in
  let uri = Uri.of_string url in
  Client.get uri >>= fun (_, body) ->
  (Cohttp_async.Body.to_string body) >>= fun s ->
  let t = serialize_json s in
  return ((t, List.map serialize_json_update t))

let get_latest_offset updates = 
  let extract_id_from_update json =
    let open Yojson.Basic.Util in
    json |> member "update_id" |> to_int
  in
  updates
  |> List.map extract_id_from_update
  |> List.fold_left max 0
  |> (+) 1
  |> string_of_int

let get_send_message_url (Bot tok) =
  base_api_url ^ tok ^ "/sendMessage"

let get_reply_to_message_url = get_send_message_url

let get_send_message_body (chat_id: string) (message: string) =
  let open Yojson.Basic in
  let res =
    `Assoc [ ("chat_id", `String chat_id)
           ; ("text"   , `String message)
           ]
  in
  let _ = print_endline (to_string res) in
  res

let get_send_formatted_message_body (chat_id: string) (message: string) (format: format_style) =
  let open Yojson.Basic in
  let res =
    `Assoc [ ("chat_id"   , `String chat_id)
           ; ("text"      , `String message)
           ; ("parse_mode", `String (format_option_value format) )
           ]
  in
  let _ = print_endline (to_string res) in
  res

let get_reply_to_message_body (chat_id: string) (msg_id: int) (message: string) =
  let open Yojson.Basic in
  let res =
    `Assoc [ ("chat_id"            , `String chat_id)
           ; ("reply_to_message_id", `Int msg_id)
           ; ("text"               , `String message)
           ]
  in
  let _ = print_endline (to_string res) in
  res

let get_reply_formatted_message_body (chat_id: string) (msg_id: int) (message: string) (format: format_style) =
  let open Yojson.Basic in
  let res =
    `Assoc [ ("chat_id"            , `String chat_id)
           ; ("text"               , `String message)
           ; ("reply_to_message_id", `Int msg_id)
           ; ("parse_mode"         , `String (format_option_value format) )
           ]
  in
  let _ = print_endline (to_string res) in
  res

let action_performer bot action =
  let send_req_and_report body headers uri = 
    Body.to_string body >>= fun body_as_string ->
    let _ = print_endline ("Body_as_string: " ^ body_as_string) in
    Client.post ~body ~headers uri >>= fun (resp, body) ->
      let status_msg = 
        resp
        |> Cohttp.Response.status
        |> Cohttp.Code.sexp_of_status_code
        |> Sexplib.Sexp.to_string
      in
      Cohttp_async.Body.to_string body >>= fun s ->
      print_endline status_msg;
      print_endline s;
      return ()
  in
  match action with
  | NoAction -> return ()
  | SendMessage( chat_id, message ) ->
      let headers = 
        Cohttp.Header.of_list [ "Content-Type", "application/json" ]
      in
      let url = get_send_message_url bot in
      let uri = Uri.of_string url in
      let body =
        (get_send_message_body chat_id message) 
        |> Yojson.Basic.to_string
        |> Body.of_string
      in
      send_req_and_report body headers uri
  | SendFormattedMessage(chat_id, text, format) ->
      let headers = 
        Cohttp.Header.of_list [ "Content-Type", "application/json" ]
      in
      let url = get_send_message_url bot in
      let uri = Uri.of_string url in
      let body =
        (get_send_formatted_message_body chat_id text format) 
        |> Yojson.Basic.to_string
        |> Body.of_string
      in
      send_req_and_report body headers uri
  | ReplyToMessage(chat_id, msg_id, text) ->
      let headers = 
        Cohttp.Header.of_list [ "Content-Type", "application/json" ]
      in
      let url = get_reply_to_message_url bot in
      let uri = Uri.of_string url in
      let body =
        (get_reply_to_message_body chat_id msg_id text) 
        |> Yojson.Basic.to_string
        |> Body.of_string
      in
      send_req_and_report body headers uri
  | ReplyToMessageFormatted(chat_id, msg_id, text, format) ->
      let headers = 
        Cohttp.Header.of_list [ "Content-Type", "application/json" ]
      in
      let url = get_reply_to_message_url bot in
      let uri = Uri.of_string url in
      let body =
        (get_reply_formatted_message_body chat_id msg_id text format) 
        |> Yojson.Basic.to_string
        |> Body.of_string
      in
      send_req_and_report body headers uri

let perform_actions bot actions =
  let f = 
    action_performer bot
  in
  List.map f actions

let poll_bot (Bot tok) f =
  let module Sleep = Async_unix.Require_explicit_time_source.Time_ns in
  let rec run offset () =
    get_updates offset tok >>= fun (raw, updates) ->
    let latest_offset = if updates != [] then
      let actions = List.map f updates in
      let _offset = get_latest_offset raw in
      let _ = print_endline (`List raw |> Yojson.Basic.pretty_to_string) in
      let _ = perform_actions (Bot tok) actions in
      Some _offset
    else 
      offset
    in
    Sleep.pause (Time_ns_unix.Span.of_ms 2000.0);
    (run [@tailcall]) latest_offset ()
  in 
  ()
  |> run None
  |> ignore
  |> return

let start tok f =
  let bot = create_bot tok in
  poll_bot bot f |> ignore;
  Core.never_returns (Scheduler.go ())