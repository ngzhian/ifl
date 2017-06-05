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

let main_entry = "main" (* entry point of program is a supercombinator called main *)

(** Helper methods *)

(* Gets the list of names (lhs) of a list of definitions *)
let binders_of (defns : (name * name expr) list) = List.map fst defns
(* Gets the list of rhs of a list of definitions *)
let rhssOf (defns : (name * name expr) list) = List.map snd defns

(* Is the expression atomic (no internal structure) *)
let is_atomic_expr (e : 'a expr) : bool = match e with
  | EVar _ -> true
  | ENum _ -> true
  | _      -> false

let true_tag = 2
and false_tag = 1

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
                            EVar "f"));
    ("False", [], EConstr (false_tag, 0));
    ("True", [], EConstr (true_tag, 0));
    ("MkPair", [], EConstr (1, 2));
    ("Nil", [], EConstr (1, 0));
    ("Cons", [], EConstr (2, 2));
  ]
let extra_prelude_defs = [
  ("and", ["x"; "y"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "y"), EVar "False"));
  ("or", ["x"; "y"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "x"), EVar "y"));
  ("not", ["x"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "False"), EVar "True"));
  ("fst", ["p"], EAp (EAp (EVar "casePair", EVar "p"), EVar "K"));
  ("snd", ["p"], EAp (EAp (EVar "casePair", EVar "p"), EVar "K1"));
  (* ("xor", ["x"; "y"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "x"), EVar "y")); *)
  (* 1 0, 1 *)
  (* 0 1, 1 *)
  (* 1 1, 0 *)
  (* 0 0, 0 *)
]
let prelude_names = List.map (fun (name, _, _) -> name) prelude_defs
