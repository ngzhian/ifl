open Ast

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
let i_line_btwn f xs = i_interleave i_newline (List.map f xs)

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

