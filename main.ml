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
let prelude_defs : core_program =
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
let extra_prelude_defs = []

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
let i_nil : iseq = INil
let i_str (s : string) : iseq = IStr s
let i_num (i : int) : iseq = i_str (string_of_int i)
let i_fwnum width n : iseq =
  let digits = string_of_int n in
  i_str (space (width - String.length digits) ^ digits)
let i_append (seq1 : iseq) (seq2 : iseq) : iseq = IAppend (seq1, seq2)
(* newline followed by a number of spaces up to current indentation *)
let i_newline : iseq = INewline
(* indents to line up with current column *)
let i_indent (seq : iseq) : iseq = IIndent seq
let i_concat (seqs : iseq list) : iseq =
  List.fold_right (fun a b -> i_append a b) seqs i_nil
let rec i_interleave (sep : iseq) (seqs : iseq list) : iseq = match seqs with
  | [] -> i_nil
  | seq :: [] -> seq
  | seq :: seqs -> i_append (i_append seq sep) (i_interleave sep seqs)
(* not sure what to do for i_layn, not described *)
let i_layn iseqs = i_interleave i_newline iseqs

let rec flatten (col : int) (seqs : (iseq * int) list) : string = match seqs with
    | [] -> ""
    | (INil, c) :: seqs -> flatten col seqs
    | (IStr s, c) :: seqs -> s ^ (flatten col seqs)
    | (IAppend (seq1, seq2), c) :: seqs -> flatten col ((seq1, col) :: (seq2, col) :: seqs)
    | (INewline, indent) :: seqs -> "\n" ^ (space indent) ^ (flatten indent seqs)
    | (IIndent seq, indent) :: seqs -> flatten col ((seq, col) :: seqs)

let i_display (seq : iseq) : string = flatten 0 [(seq, 0)]

(* Pretty print expression using iseq *)
let rec ppr_expr (e : core_expr) : iseq = match e with
    | EVar v -> i_str v
    | ENum n -> i_num n
    | EConstr (_, _) -> i_str "TODO constructor"
    | EAp (e1, e2) -> i_append (i_append (ppr_expr e1) (i_str " ")) (ppr_aexpr e2)
    | ELet (isrec, defns, expr) ->
      let keyword = if isrec then "letrec" else "let" in
      i_concat [ i_str keyword; i_newline;
                i_str "  "; i_indent (ppr_defns defns); i_newline;
                i_str "in "; ppr_expr expr ]
    | ECase (e, alts) ->
      i_concat [ i_str "case "; ppr_expr e; i_str " of"; i_newline;
                i_indent (ppr_alts alts); i_newline
              ]
    | ELam (vs, e) ->
      i_concat [ i_str "\ "; ppr_vars vs; i_str " . "; ppr_expr e ]

(* Pretty print expression using iseq *)
and ppr_aexpr (e : core_expr) : iseq =
  if is_atomic_expr e
  then ppr_expr e
  else i_append (i_append (i_str "(") (ppr_expr e)) (i_str ")")

(* Pretty print definitions *)
and ppr_defns (defns : (name * name expr) list) =
  let sep = i_concat [ i_str ";"; i_newline ] in
  i_interleave sep (List.map ppr_defn defns)

(* Pretty print a definition *)
and ppr_defn ((name : name), (expr : name expr)) =
  i_concat [ i_str name; i_str " = "; i_indent (ppr_expr expr)]

and ppr_alts (alts: (name alter list)) : iseq =
  let sep = i_concat [ i_str ";"; i_newline ] in
  i_interleave sep (List.map ppr_alt alts)

and ppr_alt ((tag, vars, e) : name alter) : iseq =
  i_concat [ i_str "<"; i_str (string_of_int tag); i_str "> ";
            ppr_vars vars; i_str "-> "; ppr_expr e;
          ]

and ppr_vars (vars : name list) : iseq =
  i_interleave (i_str " ") (List.map i_str vars)

(* Pretty print program *)
and ppr_program (prog : core_program) : iseq =
  i_interleave i_newline (List.map ppr_sc prog)

and ppr_sc ((name : name), (vars : name list), (expr : core_expr)) : iseq =
  i_concat [ i_str name; i_str " "; ppr_vars vars; i_str " = "; ppr_expr expr ]

(* Pretty print program *)
let pprint prog = i_display (ppr_program prog)

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
  let pPackVar toks = p_then3 mk_pack_var p_num (p_lit ",") p_num toks in
  let mk_pack _ _ (tag, arity) _ = EConstr (tag, arity) in
  p_then4 mk_pack (p_lit "Pack") (p_lit "{") pPackVar (p_lit "}") toks
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
  p_alt (p_then found_op (p_lit "|") p_expr3) (p_empty NoOp) toks
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

let syntax (tokens : token list) : core_program =
  let rec take_first_parse = function
    | (prog, []) :: others -> prog
    | parse :: others -> take_first_parse others
    | _ -> failwith "Syntax error"
  in
  take_first_parse (p_program tokens)

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

(* Template instantiation machine *)

(* Machine state *)
type ti_state = ti_stack * ti_dump * ti_heap * ti_globals * ti_stats
(* An address into the heap *)
and addr = int
(* Stack of addresses being worked on, our spine *)
and ti_stack = addr list
(* Dump of stacks when working on arguments *)
and ti_dump = Dummy
(* Heap to store nodes of our graph *)
and ti_heap = node list
(* Types of nodes in our TI graph *)
and node =
  | NAp of addr * addr                           (* application *)
  | NSupercomb of name * (name list) * core_expr (* supercombinator *)
  | NNum of int
(* Mapping of names (supercombinators) to addresses in heap *)
and ti_globals = (name * addr) list
(* Stats for execution of the machine *)
and ti_stats = int

(* Heap operations *)
let ti_heap_initial = []
(* allocate a node onto the heap by adding to the end *)
(* TODO slow but simple *)
let ti_heap_alloc (heap : ti_heap) (node : node) =
  (heap @ [node], List.length heap)
let ti_heap_lookup (heap : ti_heap) (addr : addr) =
  List.nth heap addr

let ti_dump_init = Dummy

let ti_stat_init = 0
let ti_stat_inc s = s + 1
let ti_stat_get s = s
let apply_to_stats fn (s, d, h, f, stats) =
  (s, d, h, f, fn stats)

(* Maps a list by calling f on the acc and elements of the list
 * returning the final acc and the input list transformed by f *)
let rec mapAccuml f acc xs =
  let rec go acc xs ys = match xs with
    | [] -> acc, ys
    | x::xs -> let (acc', y) = f acc x in go acc' xs (y::ys) in
  let (acc, ys) = go acc xs [] in
  (acc, List.rev ys)

(* Allocates a supercombinator definition on the heap *)
let allocate_sc heap (name, args, body) =
  let (heap', addr) = ti_heap_alloc heap (NSupercomb (name, args, body)) in
  (heap', (name, addr))

(* Build initial heap from definitions *)
let build_initial_heap defs = mapAccuml allocate_sc ti_heap_initial defs

(* Compile Core program *)
let compile (p : core_program) : ti_state =
  (* gather up all supercombinator defs and prelude *)
  let sc_defs = p @ prelude_defs @ extra_prelude_defs in
  (* build initial heap mapping addresses to nodes *)
  let (heap, globals) = build_initial_heap sc_defs in
  let main = List.assoc "main" globals in
  let stack = [main] in
  (stack, ti_dump_init, heap, globals, ti_stat_init)

let is_data_node (n : node) = match n with
  | NNum _ -> true
  | _ -> false

(* Detects final state *)
let ti_final (stack, dump, heap, globals, stats) = match stack with
  | [addr] -> is_data_node (ti_heap_lookup heap addr)
  | []     -> failwith "Empty stack"
  | state  -> false

(* creates instance of expression on the heap *)
let rec instantiate body heap (env : ti_globals) : (ti_heap * addr) = match body with
  | ENum n -> ti_heap_alloc heap (NNum n)
  | EAp (e1, e2) ->
    let (heap1, a1) = instantiate e1 heap env in
    let (heap2, a2) = instantiate e2 heap1 env in
    ti_heap_alloc heap2 (NAp (a1, a2))
  | EVar v -> (heap, List.assoc v env)
  | EConstr (tag, arity) ->
    instantiate_constr tag arity heap env
  | ELet (isrec, defs, body) ->
    instantiate_let isrec defs body heap env
  | ECase _ -> failwith "cant handle case expr"
  | _ -> failwith "dont know how to instantiate"
and instantiate_constr tag arity heap env =
  failwith "instantiate_constr not impl"
and instantiate_let tag arity heap env =
  failwith "instantiate_let not impl"

let rec drop n ls = match n with
  | 0 -> ls
  | n -> drop (n - 1) (List.tl ls)

let num_step state n = failwith "Number applied as function"
(* unwind rule *)
let ap_step (stack, dump, heap, globals, stats) a1 a2 =
  (a1 :: stack, dump, heap, globals, stats)
let sc_step (stack, dump, heap, globals, stats) sc args body =
  (* get address of all args on the stack *)
  let getargs heap (sc::stack) =
    List.map (fun addr -> match ti_heap_lookup heap addr with
        | NAp (f, arg) -> arg
        | _ -> failwith "error getargs")
        stack in
  (* bind arg names to addresses *)
  let arg_bindings = List.combine args (getargs heap stack) in
  let env = arg_bindings @ globals in
  (* instantiate body *)
  let (heap', result_addr) = instantiate body heap env in
  (* discard argument from stack, push result on stack *)
  let _ = print_endline ("pushing result addr: " ^ (string_of_int result_addr)) in
  (* + 1 to drop the sc node from stack *)
  let stack' = result_addr :: (drop (List.length args + 1) stack) in
  (stack', dump, heap', globals, stats)

let dispatch state (n : node) = match n with
  | NAp (a1, a2) -> ap_step state a1 a2
  | NSupercomb (sc, args, body) -> sc_step state sc args body
  | NNum n -> num_step state n

let step (state : ti_state) =
  let (stack, dump, heap, globals, stats) = state in
  (dispatch state (ti_heap_lookup heap (List.hd stack)))
  |> apply_to_stats ti_stat_inc

(* maybe do some admin on state *)
let doAdmin x = x

let rec eval (state : ti_state) : ti_state list =
  let rest_states =
    if ti_final state
    then []
    else eval (doAdmin (step state))
  in
  state :: rest_states

let last ls = List.nth ls (List.length ls - 1)

(* Facilities for printing the stack *)
let show_state (stack, _, heap, _, _) =
  let show_fw_addr addr =
    i_fwnum 4 addr in
  let show_addr addr = i_num addr in
  let show_node node = match node with
    | NAp (a1, a2) -> i_concat [ i_str "NAp "; show_addr a1;
                                i_str " "; show_addr a2 ]
    | NSupercomb (name, args, body) -> i_str ("NSupercomb " ^ name)
    | NNum n -> i_append (i_str "NNum ") (i_num n) in
  let show_stack_node heap node = match node with
    | NAp (faddr, argaddr) -> i_concat [ i_str "NAp "; show_fw_addr faddr;
                                        i_str " "; show_fw_addr argaddr; i_str " (";
                                        show_node (ti_heap_lookup heap argaddr);
                                        i_str ")" ]
    | node -> show_node node in
  let show_stack_item addr = i_concat [ show_fw_addr addr; i_str ": ";
                                       show_stack_node heap (ti_heap_lookup heap addr)] in
  let show_stack heap stack =
    i_concat [ i_str "Stk [";
              i_indent (i_interleave i_newline (List.map show_stack_item stack));
              i_str "]" ] in
  i_concat [ show_stack heap stack; i_newline ]
let showStats (_, _, _, _, stats) =
  i_concat [ i_newline; i_newline; i_str "Total number of steps = ";
            i_num (ti_stat_get stats) ]
let show_results (states : ti_state list) : string =
  i_display (i_concat [ i_layn (List.map show_state states);
                      showStats (last states)
                    ])

let run_prog_string s =
  s |> parse_string |> compile |> eval |> show_results
let run_prog filename =
  filename |> parse |> compile |> eval |> show_results

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
