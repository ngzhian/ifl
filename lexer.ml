open Ast

(** Lexer *)

type token = int * char list

let is_whitespace c = c = ' ' || c = '\t' || c = '\n'
let is_digit c = c >= '0' &&  c <= '9'
let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_idchar c = is_alpha c || is_digit c || c = '_'
let rec take_while p (cs : char list) = match cs with
  | [] -> []
  | c :: cs -> if p c then c :: take_while p cs else []
let rec drop_while p (cs : char list) = match cs with
  | [] -> []
  | c :: cs' -> if p c then drop_while p cs' else cs
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
let string_of_chars chars =
    let buf = Buffer.create (List.length chars) in
      List.iter (Buffer.add_char buf) chars;
        Buffer.contents buf

let two_char_ops = [ "=="; "~="; ">="; "<="; "=>" ]

let clex (input : string) : token list =
  let rec _clex (ln : int) (chars : char list) : token list =
    match chars with
    | [] -> []
    (* comments begin with || and extends till new line *)
    | '|' :: '|' :: cs -> _clex ln (drop_while (fun c -> c != '\n') cs)
    (* special two char ops we want to lex as a token *)
    | c :: c' :: cs when List.mem (string_of_chars [c; c']) two_char_ops ->
      (ln, [c; c']) :: _clex ln cs
    (* special case new line to report line numbers accurately *)
    | '\n' :: cs -> _clex (ln + 1) cs
    | c :: cs when is_whitespace c -> _clex ln cs
    | c :: cs when is_digit c ->
      let num_token = c :: take_while is_digit cs in
      let rest_cs = drop_while is_digit cs in
      (ln, num_token) :: _clex ln rest_cs
    | c :: cs when is_alpha c ->
      let var_tok = c :: take_while is_idchar cs in
      let rest_cs = drop_while is_idchar cs in
      (ln, var_tok) :: _clex ln rest_cs
    | c :: cs -> (ln, [c]) :: _clex ln cs
  in _clex 0 (explode input)

