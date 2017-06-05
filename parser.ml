open Ast
open Lexer

(** Parser *)

(**
        parser                 parsed result
        |       input tokens   |    unconsumed tokens
        |       |              |    |           possible parses
 *      V       V              V    V           V               *)
type 'a parser = token list -> ('a * token list) list

let p_sat (p : string -> bool) : string parser =
  function
  | ((_, cs)::toks) when p (string_of_chars cs) -> [((string_of_chars cs), toks)]
  | (_::_) -> []
  | [] -> []

(* Parsers a literal, string, by comparing the first token on the inpu with a string *)
let p_lit (s : string) : string parser =
  p_sat (fun x -> x = s)
(* let p_lit (s : string) : string parser = *)
(*   function *)
(*   | ((_, cs)::toks) when string_of_chars cs = s -> [(s, toks)] *)
(*   | (_::_) -> [] *)
(*   | [] -> [] *)

let keywords = ["let"; "letrec"; "case"; "in"; "of"; "Pack"]
let p_var : string parser =
  p_sat (fun x -> is_alpha x.[0] && not (List.mem x keywords))
(* let p_var : string parser = *)
(*   function *)
(*   | ((_, c::cs)::toks) when is_alpha c -> [(string_of_chars (c :: cs), toks)] *)
(*   | ((_, _)::toks) -> [] *)
(*   | [] -> [] *)

let p_apply (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun toks ->
    List.map (fun (x, toks) -> (f x, toks) ) (p toks)

let p_num : int parser =
  p_apply
  (p_sat (fun x -> List.for_all is_digit (explode x)))
  int_of_string

let p_alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun tokens -> (p1 tokens) @ (p2 tokens)

let p_then (combine : 'a -> 'b -> 'c) (p1 : 'a parser) (p2 : 'b parser) : 'c parser =
  function
  | toks ->
    List.concat
      (* parse using p1 *)
      (List.map
         (* then for each p1 parse result, parse using p2 *)
         (fun (v1, toks1) ->
            (List.map
               (fun (v2, toks2) ->
                  (* and combine the results *)
                  (combine v1 v2, toks2))
               (p2 toks1)))
         (p1 toks))

let p_then3 (combine : 'a -> 'b -> 'c -> 'd)
           (p1 : 'a parser) (p2 : 'b parser) (p3 : 'c parser) : 'd parser =
  function
  | toks ->
    List.concat
      (List.map
         (fun (v1, toks1) ->
            List.concat
              (List.map
                 (fun (v2, toks2) ->
                    (List.map
                       (fun (v3, toks3) ->
                          (combine v1 v2 v3, toks3))
                       (p3 toks2)))
                 (p2 toks1)))
         (p1 toks))

let p_then4 (combine : 'a -> 'b -> 'c -> 'd -> 'e)
           (p1 : 'a parser) (p2 : 'b parser) (p3 : 'c parser) (p4 : 'd parser) : 'e parser =
  function
  | toks ->
    List.concat
      (List.map
         (fun (v1, toks1) ->
            List.concat
              (List.map
                 (fun (v2, toks2) ->
                    List.concat
                      (List.map
                         (fun (v3, toks3) ->
                            List.map
                              (fun (v4, toks4) ->
                                 (combine v1 v2 v3 v4, toks4))
                              (p4 toks3))
                         (p3 toks2)))
                 (p2 toks1)))
         (p1 toks))

let p_empty (v : 'a) : 'a parser = fun toks -> [(v, toks)]

let cons hd tl = hd :: tl

(* maybe can work on making this only return the greediest parse *)
let rec p_one_or_more (p : 'a parser) : 'a list parser =
  (* consumes tokens greedily, tries to parse as many as possible *)
  fun toks ->
    let r = p_then cons p (fun toks -> p_zero_or_more p toks) toks in
    match r with
    | [] -> p_then cons p (p_empty []) toks
    | _ -> r
and oldpOneOrMore (p : 'a parser) : 'a list parser =
  (* this is an old implementation that leads to exponential increase in possible parses *)
  p_alt
  (* p_then cons p (p_zero_or_more p)
   * doesn't work because of infinite recursion,
   * the key is to abstract it so that it's not eagerly evaluated
   * *)
  (p_then cons p (fun toks -> p_zero_or_more p toks))
  (p_then cons p (p_empty []))
and p_zero_or_more (p : 'a parser) : 'a list parser =
  (* greedy parsing, parses as many p as possible before returning empty *)
  fun toks ->
    match p_one_or_more p toks with
    | [] -> (p_empty [] toks)
    | r -> r
  (* p_alt (p_one_or_more p) (p_empty []) *)

let rec p_one_or_more_with_sep (p1 : 'a parser) (p2 : 'b parser) : 'a list parser =
  let add3 hd sep tl = hd :: tl in
  p_alt
    (p_then cons p1 (p_empty []))
    (p_then3 add3 p1 p2 (fun toks -> p_one_or_more_with_sep p1 p2 toks))

type partial_expr = NoOp | FoundOp of name * core_expr

let rec p_expr toks =
  List.fold_left
    (fun a b -> p_alt a b)
    p_aexpr (* p_evar and p_enum *)
    [
      p_elet; p_eletrec;
      p_ecase;
      p_elam;
      p_expr1;
      p_pack;
    ]
  toks
and p_evar =
  p_apply p_var (fun v -> EVar v)
and p_enum =
  p_apply p_num (fun n -> ENum n)
and p_elet toks =
  let mk_let l defs _ e = ELet (non_recursive, defs, e) in
  (p_then4 mk_let (p_lit "let") p_defs (p_lit "in") p_expr) toks
and p_eletrec toks =
  let mk_let l defs _ e = ELet (recursive, defs, e) in
  (p_then4 mk_let (p_lit "letrec") p_defs (p_lit "in") p_expr) toks
and p_defs toks =
  p_one_or_more_with_sep p_def (p_lit ";") toks
and p_def toks =
  let mk_def v _ e = (v, e) in
  p_then3 mk_def p_var (p_lit "=") p_expr toks
and p_ecase toks =
  let mk_case _ e _ alters = ECase (e, alters) in
  p_then4 mk_case (p_lit "case") p_expr (p_lit "of") p_alters toks
and p_alters toks =
  p_one_or_more_with_sep p_alter (p_lit ";") toks
and p_alter toks =
  let mk_alter tag vs _ e = (tag, vs, e) in
  p_then4 mk_alter p_tag (p_zero_or_more p_var) (p_arrow) p_expr toks
and p_arrow toks =
  p_then (fun _ _ -> ()) (p_lit "-") (p_lit ">") toks
and p_tag toks =
  let mk_tag _ tag _ = tag in
  p_then3 mk_tag (p_lit "<") p_num (p_lit ">") toks
and p_pexpr toks = (* parenthesized expr *)
  let mk_expr _ e _ = e in
  p_then3 mk_expr (p_lit "(") p_expr (p_lit ")") toks
and p_pack toks =
  let mk_pack_var tag _ arity = (tag, arity) in
  let p_packvar toks = p_then3 mk_pack_var p_num (p_lit ",") p_num toks in
  let mk_pack _ _ (tag, arity) _ = EConstr (tag, arity) in
  p_then4 mk_pack (p_lit "Pack") (p_lit "{") p_packvar (p_lit "}") toks
and p_aexpr toks = (* atomic expressions *)
  p_alt (p_alt p_evar p_enum) p_pexpr toks
and p_elam toks =
  let mk_lam _ vs _ e = ELam (vs, e) in
  p_then4 mk_lam (p_lit "\\") (p_one_or_more p_var) (p_lit ".") p_expr toks
and p_eap toks =
  let mk_ap_chain (hd::tl) =
    List.fold_left (fun a b -> EAp (a, b)) hd tl in
  p_apply (p_one_or_more p_aexpr) mk_ap_chain toks
and found_op n e = FoundOp (n, e)
and p_expr1 toks =
  p_then assemble_op p_expr2 p_expr1c toks
and p_expr1c toks =
  p_alt (p_then found_op (p_lit "|") p_expr1) (p_empty NoOp) toks
and p_expr2 toks =
  p_then assemble_op p_expr3 p_expr2c toks
and p_expr2c toks =
  p_alt (p_then found_op (p_lit "&") p_expr2) (p_empty NoOp) toks
and p_expr3 toks =
  p_then assemble_op p_expr4 p_expr3c toks
and p_expr3c toks =
  p_alt (p_then found_op p_relop p_expr3) (p_empty NoOp) toks
and p_relop toks =
  let ops = List.map p_lit ("<"::two_char_ops) in
  (List.fold_left (fun a b -> p_alt a b) (p_lit ">") ops) toks
and p_expr4 toks =
  p_then assemble_op p_expr5 p_expr4c toks
and p_expr4c toks =
  p_alt
    (p_alt (p_then found_op (p_lit "+") p_expr4) (p_empty NoOp))
    (p_then found_op (p_lit "-") p_expr4)
    toks
and p_expr5 toks =
  p_then assemble_op p_expr6 p_expr5c toks
and p_expr5c toks =
  p_alt
    (p_alt (p_then found_op (p_lit "*") p_expr5) (p_empty NoOp))
    (p_then found_op (p_lit "/") p_expr5)
    toks
and p_expr6 toks =
  p_eap toks
and assemble_op e1 = function
  | NoOp             -> e1
  | FoundOp (op, e2) -> EAp (EAp (EVar op, e1), e2)

let p_sc : core_sc_defn parser =
  let mk_sc v vs _ e = (v, vs, e) in
  p_then4 mk_sc p_var (p_zero_or_more p_var) (p_lit "=") p_expr

let p_program : core_program parser =
  p_one_or_more_with_sep p_sc (p_lit ";")

