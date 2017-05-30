(** The AST of Core language *)

type 'a expr =
    EVar of name                 (* Variables *)
  | ENum of int                  (* Numbers *)
  | EConstr of int * int         (* Constructor tag arity *)
  | EAp of ('a expr) * ('a expr) (* Applications *)
  | ELet of                      (* Let(rec) expressions *)
      isRec *                    (* boolean with True = recursive *)
      ('a * 'a expr) list *      (* Definitions *)
      ('a expr)                  (* Body of let(rec) *)
  | ECase of                     (* Case expressions *)
      ('a expr) *                (* Expression to match on *)
      'a alter list              (* Alternatives *)
  | ELam of 'a list * 'a expr    (* Lambda abstractions *)

and name = string              (* Variable name as string of characters *)

and isRec = bool               (* Is a let(rec) recursive *)

and 'a alter =                 (* Alternatives in a case expression *)
  int * 'a list * 'a expr      (* Tag, bound variables, expression right of arrow *)

let recursive : isRec = true      (* let is recursive *)
let non_recursive : isRec = false (* let is not recursive *)

type core_expr = name expr      (* core_expr where the binder of expr are name *)
and core_alt   = name alter

type 'a program = 'a sc_defn list         (* a core program is a list of sc definitions *)
and core_program = name program
and 'a sc_defn = name * 'a list * 'a expr (* name, arguments, body *)
and core_sc_defn = name sc_defn

(** Helper methods *)

(* Gets the list of names (lhs) of a list of definitions *)
let bindersOf (defns : (name * name expr) list) = List.map fst defns
(* Gets the list of rhs of a list of definitions *)
let rhssOf (defns : (name * name expr) list) = List.map snd defns

(* Is the expression atomic (no internal structure) *)
let is_atomic_expr (e : 'a expr) : bool = match e with
  | EVar _ -> true
  | ENum _ -> true
  | _      -> false

(* Prelude (pervasives) to be included in all Core programs *)
let preludeDefs : core_program =
  [ ("I",  ["x"], EVar "x");
    ("K",  ["x"; "y"], EVar "x");
    ("K1", ["x"; "y"], EVar "y");
    ("S",  ["f"; "g"; "x"], EAp (EAp (EVar "f", EVar "x"),
                                 EAp (EVar "g", EVar "x")));
    ("compose", ["f"; "g"; "x"], EAp (EVar "f",
                                      EAp (EVar "g", EVar "x")));
    ("twice",   ["f"], EAp (EAp (EVar "compose", EVar "f"),
                            EVar "f"))
  ]

(* slow pretty print functions based on string concatenation *)
(* Pretty print core expressions *)
(* let rec spprExpr (e : core_expr) : string = match e with *)
(*     | EVar v -> v *)
(*     | ENum n -> string_of_int n *)
(*     | EConstr (_,_) -> (??) *)
(*     | EAp (e1, e2) -> spprExpr e1 ^ " " ^ spprExpr e2 *)
(*     | ELet (_,_,_) -> (??) *)
(*     | ECase (_,_) -> (??) *)
(*     | ELam (_,_) -> (??) *)

(* Like spprExpr, but wrap parentheses unless expression is atomic *)
(* let rec spprAExpr (e : core_expr) : string = *)
(*   if is_atomic_expr e *)
(*   then spprExpr e *)
(*   else "(" ^ spprExpr e ^ ")" *)

(** Pretty printing facilities based on iseq *)

type iseq =
    INil
  | IStr of string
  | IAppend of iseq * iseq
  | IIndent of iseq
  | INewline

let space indent = String.make indent ' '
let iNil : iseq = INil
let iStr (s : string) : iseq = IStr s
let iNum (i : int) : iseq = iStr (string_of_int i)
let iFWNum width n : iseq =
  let digits = string_of_int n in
  iStr (space (width - String.length digits) ^ digits)
let iAppend (seq1 : iseq) (seq2 : iseq) : iseq = IAppend (seq1, seq2)
(* newline followed by a number of spaces up to current indentation *)
let iNewline : iseq = INewline
(* indents to line up with current column *)
let iIndent (seq : iseq) : iseq = IIndent seq
let iConcat (seqs : iseq list) : iseq =
  List.fold_right (fun a b -> iAppend a b) seqs iNil
let rec iInterleave (sep : iseq) (seqs : iseq list) : iseq = match seqs with
  | [] -> iNil
  | seq :: [] -> seq
  | seq :: seqs -> iAppend (iAppend seq sep) (iInterleave sep seqs)

let rec flatten (col : int) (seqs : (iseq * int) list) : string = match seqs with
    | [] -> ""
    | (INil, c) :: seqs -> flatten col seqs
    | (IStr s, c) :: seqs -> s ^ (flatten col seqs)
    | (IAppend (seq1, seq2), c) :: seqs -> flatten col ((seq1, col) :: (seq2, col) :: seqs)
    | (INewline, indent) :: seqs -> "\n" ^ (space indent) ^ (flatten indent seqs)
    | (IIndent seq, indent) :: seqs -> flatten col ((seq, col) :: seqs)

let iDisplay (seq : iseq) : string = flatten 0 [(seq, 0)]

(* Pretty print expression using iseq *)
let rec pprExpr (e : core_expr) : iseq = match e with
    | EVar v -> iStr v
    | ENum n -> iNum n
    | EConstr (_, _) -> iStr "TODO constructor"
    | EAp (e1, e2) -> iAppend (iAppend (pprExpr e1) (iStr " ")) (pprAExpr e2)
    | ELet (isrec, defns, expr) ->
      let keyword = if isrec then "letrec" else "let" in
      iConcat [ iStr keyword; iNewline;
                iStr "  "; iIndent (pprDefns defns); iNewline;
                iStr "in "; pprExpr expr ]
    | ECase (e, alts) ->
      iConcat [ iStr "case "; pprExpr e; iStr " of"; iNewline;
                iIndent (pprAlts alts); iNewline
              ]
    | ELam (vs, e) ->
      iConcat [ iStr "\ "; pprVars vs; iStr " . "; pprExpr e ]

(* Pretty print expression using iseq *)
and pprAExpr (e : core_expr) : iseq =
  if is_atomic_expr e
  then pprExpr e
  else iAppend (iAppend (iStr "(") (pprExpr e)) (iStr ")")

(* Pretty print definitions *)
and pprDefns (defns : (name * name expr) list) =
  let sep = iConcat [ iStr ";"; iNewline ] in
  iInterleave sep (List.map pprDefn defns)

(* Pretty print a definition *)
and pprDefn ((name : name), (expr : name expr)) =
  iConcat [ iStr name; iStr " = "; iIndent (pprExpr expr)]

and pprAlts (alts: (name alter list)) : iseq =
  let sep = iConcat [ iStr ";"; iNewline ] in
  iInterleave sep (List.map pprAlt alts)

and pprAlt ((tag, vars, e) : name alter) : iseq =
  iConcat [ iStr "<"; iStr (string_of_int tag); iStr "> ";
            pprVars vars; iStr "-> "; pprExpr e;
          ]

and pprVars (vars : name list) : iseq =
  iInterleave (iStr " ") (List.map iStr vars)

(* Pretty print program *)
and pprProgram (prog : core_program) : iseq =
  iInterleave iNewline (List.map pprSc prog)

and pprSc ((name : name), (vars : name list), (expr : core_expr)) : iseq =
  iConcat [ iStr name; iStr " "; pprVars vars; iStr " = "; pprExpr expr ]

(* Pretty print program *)
let pprint prog = iDisplay (pprProgram prog)

(** Lexer *)

type token = int * char list

let isWhitespace c = c = ' ' || c = '\t' || c = '\n'
let isDigit c = c >= '0' &&  c <= '9'
let isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let isIdChar c = isAlpha c || isDigit c || c = '_'
let rec takeWhile p (cs : char list) = match cs with
  | [] -> []
  | c :: cs -> if p c then c :: takeWhile p cs else []
let rec dropWhile p (cs : char list) = match cs with
  | [] -> []
  | c :: cs' -> if p c then dropWhile p cs' else cs
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
let string_of_chars chars =
    let buf = Buffer.create (List.length chars) in
      List.iter (Buffer.add_char buf) chars;
        Buffer.contents buf

let twoCharOps = [ "=="; "~="; ">="; "<="; "=>" ]

let clex (input : string) : token list =
  let rec _clex (ln : int) (chars : char list) : token list =
    match chars with
    | [] -> []
    (* comments begin with || and extends till new line *)
    | '|' :: '|' :: cs -> _clex ln (dropWhile (fun c -> c != '\n') cs)
    (* special two char ops we want to lex as a token *)
    | c :: c' :: cs when List.mem (string_of_chars [c; c']) twoCharOps ->
      (ln, [c; c']) :: _clex ln cs
    (* special case new line to report line numbers accurately *)
    | '\n' :: cs -> _clex (ln + 1) cs
    | c :: cs when isWhitespace c -> _clex ln cs
    | c :: cs when isDigit c ->
      let num_token = c :: takeWhile isDigit cs in
      let rest_cs = dropWhile isDigit cs in
      (ln, num_token) :: _clex ln rest_cs
    | c :: cs when isAlpha c ->
      let var_tok = c :: takeWhile isIdChar cs in
      let rest_cs = dropWhile isIdChar cs in
      (ln, var_tok) :: _clex ln rest_cs
    | c :: cs -> (ln, [c]) :: _clex ln cs
  in _clex 0 (explode input)

(** Parser *)

(**
        parser                 parsed result
        |       input tokens   |    unconsumed tokens
        |       |              |    |           possible parses
 *      V       V              V    V           V               *)
type 'a parser = token list -> ('a * token list) list

let pSat (p : string -> bool) : string parser =
  function
  | ((_, cs)::toks) when p (string_of_chars cs) -> [((string_of_chars cs), toks)]
  | (_::_) -> []
  | [] -> []

(* Parsers a literal, string, by comparing the first token on the inpu with a string *)
let pLit (s : string) : string parser =
  pSat (fun x -> x = s)
(* let pLit (s : string) : string parser = *)
(*   function *)
(*   | ((_, cs)::toks) when string_of_chars cs = s -> [(s, toks)] *)
(*   | (_::_) -> [] *)
(*   | [] -> [] *)

let keywords = ["let"; "letrec"; "case"; "in"; "of"; "Pack"]
let pVar : string parser =
  pSat (fun x -> isAlpha x.[0] && not (List.mem x keywords))
(* let pVar : string parser = *)
(*   function *)
(*   | ((_, c::cs)::toks) when isAlpha c -> [(string_of_chars (c :: cs), toks)] *)
(*   | ((_, _)::toks) -> [] *)
(*   | [] -> [] *)

let pApply (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun toks ->
    List.map (fun (x, toks) -> (f x, toks) ) (p toks)

let pNum : int parser =
  pApply
  (pSat (fun x -> List.for_all isDigit (explode x)))
  int_of_string

let pAlt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun tokens -> (p1 tokens) @ (p2 tokens)

let pThen (combine : 'a -> 'b -> 'c) (p1 : 'a parser) (p2 : 'b parser) : 'c parser =
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

let pThen3 (combine : 'a -> 'b -> 'c -> 'd)
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

let pThen4 (combine : 'a -> 'b -> 'c -> 'd -> 'e)
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

let pEmpty (v : 'a) : 'a parser = fun toks -> [(v, toks)]

let cons hd tl = hd :: tl

(* maybe can work on making this only return the greediest parse *)
let rec pOneOrMore (p : 'a parser) : 'a list parser =
  (* consumes tokens greedily, tries to parse as many as possible *)
  fun toks ->
    let r = pThen cons p (fun toks -> pZeroOrMore p toks) toks in
    match r with
    | [] -> pThen cons p (pEmpty []) toks
    | _ -> r
and oldpOneOrMore (p : 'a parser) : 'a list parser =
  (* this is an old implementation that leads to exponential increase in possible parses *)
  pAlt
  (* pThen cons p (pZeroOrMore p)
   * doesn't work because of infinite recursion,
   * the key is to abstract it so that it's not eagerly evaluated
   * *)
  (pThen cons p (fun toks -> pZeroOrMore p toks))
  (pThen cons p (pEmpty []))
and pZeroOrMore (p : 'a parser) : 'a list parser =
  (* greedy parsing, parses as many p as possible before returning empty *)
  fun toks ->
    match pOneOrMore p toks with
    | [] -> (pEmpty [] toks)
    | r -> r
  (* pAlt (pOneOrMore p) (pEmpty []) *)

let rec pOneOrMoreWithSep (p1 : 'a parser) (p2 : 'b parser) : 'a list parser =
  let add3 hd sep tl = hd :: tl in
  pAlt
    (pThen cons p1 (pEmpty []))
    (pThen3 add3 p1 p2 (fun toks -> pOneOrMoreWithSep p1 p2 toks))

type partial_expr = NoOp | FoundOp of name * core_expr

let rec pExpr toks =
  List.fold_left
    (fun a b -> pAlt a b)
    pAExpr (* pEVar and pENum *)
    [
      pELet; pELetRec;
      pECase;
      pELam;
      pExpr1
    ]
  toks
and pEVar =
  pApply pVar (fun v -> EVar v)
and pENum =
  pApply pNum (fun n -> ENum n)
and pELet toks =
  let mk_let l defs _ e = ELet (false, defs, e) in
  (pThen4 mk_let (pLit "let") pDefs (pLit "in") pExpr) toks
and pELetRec toks =
  let mk_let l defs _ e = ELet (true, defs, e) in
  (pThen4 mk_let (pLit "letrec") pDefs (pLit "in") pExpr) toks
and pDefs toks =
  pOneOrMoreWithSep pDef (pLit ";") toks
and pDef toks =
  let mk_def v _ e = (v, e) in
  pThen3 mk_def pVar (pLit "=") pExpr toks
and pECase toks =
  let mk_case _ e _ alters = ECase (e, alters) in
  pThen4 mk_case (pLit "case") pExpr (pLit "of") pAlters toks
and pAlters toks =
  pOneOrMoreWithSep pAlter (pLit ";") toks
and pAlter toks =
  let mk_alter tag vs _ e = (tag, vs, e) in
  pThen4 mk_alter pTag (pZeroOrMore pVar) (pArrow) pExpr toks
and pArrow toks =
  pThen (fun _ _ -> ()) (pLit "-") (pLit ">") toks
and pTag toks =
  let mk_tag _ tag _ = tag in
  pThen3 mk_tag (pLit "<") pNum (pLit ">") toks
and pPExpr toks = (* parenthesized expr *)
  let mk_expr _ e _ = e in
  pThen3 mk_expr (pLit "(") pExpr (pLit ")") toks
and pPack toks =
  let mk_pack_var tag _ arity = (tag, arity) in
  let pPackVar toks = pThen3 mk_pack_var pNum (pLit ",") pNum toks in
  let mk_pack _ _ (tag, arity) _ = EConstr (tag, arity) in
  pThen4 mk_pack (pLit "Pack") (pLit "{") pPackVar (pLit "}") toks
and pAExpr toks = (* atomic expressions *)
  pAlt (pAlt pEVar pENum) pPExpr toks
and pELam toks =
  let mk_lam _ vs _ e = ELam (vs, e) in
  pThen4 mk_lam (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr toks
and pEAp toks =
  let mk_ap_chain (hd::tl) =
    List.fold_left (fun a b -> EAp (a, b)) hd tl in
  pApply (pOneOrMore pAExpr) mk_ap_chain toks
and found_op n e = FoundOp (n, e)
and pExpr1 toks =
  pThen assembleOp pExpr2 pExpr1c toks
and pExpr1c toks =
  pAlt (pThen found_op (pLit "|") pExpr1) (pEmpty NoOp) toks
and pExpr2 toks =
  pThen assembleOp pExpr3 pExpr2c toks
and pExpr2c toks =
  pAlt (pThen found_op (pLit "&") pExpr2) (pEmpty NoOp) toks
and pExpr3 toks =
  pThen assembleOp pExpr4 pExpr3c toks
and pExpr3c toks =
  pAlt (pThen found_op (pLit "|") pExpr3) (pEmpty NoOp) toks
and pExpr4 toks =
  pThen assembleOp pExpr5 pExpr4c toks
and pExpr4c toks =
  pAlt
    (pAlt (pThen found_op (pLit "+") pExpr4) (pEmpty NoOp))
    (pThen found_op (pLit "-") pExpr4)
    toks
and pExpr5 toks =
  pThen assembleOp pExpr6 pExpr5c toks
and pExpr5c toks =
  pAlt
    (pAlt (pThen found_op (pLit "*") pExpr5) (pEmpty NoOp))
    (pThen found_op (pLit "/") pExpr5)
    toks
and pExpr6 toks =
  pEAp toks
and assembleOp e1 = function
  | NoOp             -> e1
  | FoundOp (op, e2) -> EAp (EAp (EVar op, e1), e2)

let pSc : core_sc_defn parser =
  let mk_sc v vs _ e = (v, vs, e) in
  pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr

let pProgram : core_program parser =
  pOneOrMoreWithSep pSc (pLit ";")

let syntax (tokens : token list) : core_program =
  let rec take_first_parse = function
    | (prog, []) :: others -> prog
    | parse :: others -> take_first_parse others
    | _ -> failwith "Syntax error"
  in
  take_first_parse (pProgram tokens)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let parse_string s : core_program =
  syntax (clex s)

let parse filename : core_program =
  syntax (clex (load_file filename))

(* some sample programs *)
let quad = ("quad",  ["x"],
     ELet (false,
           ["twice", EAp (EAp (EVar "+", EVar "x"), EVar "x")],
           EAp (EAp (EVar "+", EVar "twice"), EVar "twice")))
let eg1 : core_program =
  [ ("I",  ["x"], EVar "x");
    ("K",  ["x"; "y"], EVar "x");
    quad
  ]

let _ = print_endline (pprint eg1)
