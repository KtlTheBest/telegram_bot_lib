open Sexplib.Std

type bot_actions =
  | NoAction
  | SendMessage of string * string
  | SendFormattedMessage of string * string * format_style
  | ReplyToMessage of string * int * string
  | ReplyToMessageFormatted of string * int * string * format_style
  | SendInlineKeyboard of string * string * keyboard

and keyboard = Yojson.Basic.t

and key_button_data =
  | CallbackData of string
  | Url of string

and key_button = 
  {
    text: string;
    data: key_button_data
  }

and format_style =
  | Html
  | MarkDown
  | MarkDownV2

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

type message_contents =
  | Empty
  | TextMessage of string
  [@@deriving sexp]

type message_update_basic = {
  message_id : string;
  message_contents_val: message_contents;
  user: user_info_basic;
  chat: chat_info_basic
} [@@deriving sexp]

type callback_query_basic = {
  message_id: string;
  user: user_info_basic;
  message: message_contents;
  chat: chat_info_basic
} [@@deriving sexp]

type update_contents =
  | MessageUpdate of message_update_basic
  | CallbackQuery of callback_query_basic
  [@@deriving sexp]

type update_info_basic = {
  update_id: int;
  update_contents: update_contents
} [@@deriving sexp]