open Ast
open Pprint
open Lexer
open Parser
open Util

(* Template instantiation machine *)

(* Machine state *)
type ti_state = ti_stack * ti_dump * ti_heap * ti_globals * ti_stats
(* An address into the heap *)
and addr = int
(* Stack of addresses being worked on, our spine *)
and ti_stack = addr list
(* Dump of stacks when working on arguments *)
and ti_dump = ti_stack list
(* Heap to store nodes of our graph *)
and ti_heap = node list
            * int       (* heap allocs *)
(* Types of nodes in our TI graph *)
and node =
  | NAp of addr * addr                           (* application *)
  | NSupercomb of name * (name list) * core_expr (* supercombinator *)
  | NNum of int
  | NInd of addr                                 (* indirection *)
  | NPrim of name * primitive                    (* primitive *)
  | NData of int * addr list                     (* tag and list of components *)
(* primitive operations the machine supports *)
and primitive =
  | Neg | Add | Sub | Mul | Div
  | PrimConstr of int * int                      (* tag, arity *)
  | If
  | Greater | GreaterEq | Less | LessEq | Eq | NotEq
  | CasePair | CaseList
  | Abort
(* Mapping of names (supercombinators) to addresses in heap *)
and ti_globals = (name * addr) list
(* Stats for execution of the machine *)
and ti_stats = int (* number of steps *)
             * int (* primitive reduction *)
             * int (* supercombinator reductions *)
             * int (* max stack depth *)

(* Heap operations *)
let ti_heap_initial = Heap.init
(* allocate a node onto the heap by adding to the end *)
(* TODO slow but simple *)
let ti_heap_alloc = Heap.alloc
(* let ti_heap_alloc ((heap, n_alloc) : ti_heap) (node : node) = *)
(*   ((heap @ [node], n_alloc + 1), List.length heap) *)
(* copy a node from addr m to addr n *)
let ti_heap_copy = Heap.copy
(* let ti_heap_copy ((heap, n_alloc)) m n : ti_heap = *)
(*   (List.mapi (fun i a -> if i = n then List.nth heap m else a) heap, n_alloc) *)
(* update a node from at addr m to node *)
let ti_heap_update  = Heap.update
(* let ti_heap_update ((heap, n_alloc)) m node : ti_heap = *)
(*   (List.mapi (fun i a -> if i = m then node else a) heap, n_alloc) *)
let ti_heap_lookup  = Heap.lookup
(* let ti_heap_lookup ((heap, n_alloc) : ti_heap) (addr : addr) = *)
(*   List.nth heap addr *)
let ti_heap_size  = Heap.size
(* let ti_heap_size heap = *)
  (* List.length heap *)
(* get all addresses on heap *)
let ti_heap_addrs ((heap, n_alloc) : ti_heap) : addr list =
  (* since heap is a list, we want to return a list of addr from 0 to length -1 *)
  let rec list_up_to acc = function
    | 0 -> acc
    | n -> list_up_to (n - 1 :: acc) (n - 1) in
  list_up_to [] (ti_heap_size heap)

let ti_dump_init = []

let ti_stat_init : ti_stats = (0, 0, 0, 0)
let ti_stat_inc (s, p, c, d) = (s + 1, p, c, d)
let ti_stat_p_r (s, p, c, d) = (s, p + 1, c, d)
let ti_stat_s_r (s, p, c, d) = (s, p, c + 1, d)
let ti_stat_depth (s, p, c, d) d' = (s, p, c, max d d')
let ti_stat_get s = s
let apply_to_stats fn (s, d, h, f, stats) =
  (s, d, h, f, fn stats)

(* Allocates a supercombinator definition on the heap *)
let allocate_sc heap (name, args, body) =
  let sc = NSupercomb (name, args, body) in
  let (heap', addr) = ti_heap_alloc heap sc in
  (heap', (name, addr))

(* Primitive operators supported in Core *)
let primitives = [
  ("if", If);
  ("negate", Neg); ("+", Add); ("-", Sub); ("*", Mul); ("/", Div);
  (">", Greater ); (">=", GreaterEq );
  ("<", Less ); ("<=", LessEq );
  ("==", Eq ); ("~=", NotEq);
  ("casePair", CasePair);
  ("caseList", CaseList);
]
(* Allocates a primitive definition on the heap *)
let allocate_prim heap (name, prim) =
  let (heap', addr) = ti_heap_alloc heap (NPrim (name, prim)) in
  (heap', (name, addr))

(* Build an initial heap from definitions *)
let build_initial_heap defs =
  let (heap1, sc_addrs) = map_accuml allocate_sc ti_heap_initial defs in
  let (heap2, prim_addrs) = map_accuml allocate_prim heap1 primitives in
  (heap2, sc_addrs @ prim_addrs)

(* Compile Core program *)
let compile (p : core_program) : ti_state =
  (* gather up all supercombinator defs and prelude *)
  let sc_defs = p @ prelude_defs @ extra_prelude_defs in
  (* build initial heap from definitions *)
  (* initial heap contains program and prelude sc nodes *)
  (* globals contains names to address mapping *)
  let (heap, globals) = build_initial_heap sc_defs in
  (* find the address of main entry point *)
  let main = List.assoc main_entry globals in
  (* begin the machine with the main entry point on stack *)
  let stack = [main] in
  (stack, ti_dump_init, heap, globals, ti_stat_init)

let is_data_node (n : node) = match n with
  | NNum _ | NData _ -> true
  | _ -> false

(* Detects final state *)
let ti_final (stack, dump, heap, globals, stats) = match stack, dump with
  | [addr], [] -> is_data_node (ti_heap_lookup heap addr)
  | [], _      -> failwith "Empty stack"
  | _, _   -> false

(* creates instance of expression on the heap *)
let rec instantiate body heap (env : ti_globals) : (ti_heap * addr) = match body with
  | ENum n -> ti_heap_alloc heap (NNum n)

  | EAp (e1, e2) ->
    let (heap1, a1) = instantiate e1 heap env in
    let (heap2, a2) = instantiate e2 heap1 env in
    ti_heap_alloc heap2 (NAp (a1, a2))

  | EVar v -> (heap, try List.assoc v env with Not_found -> failwith ("error here: " ^ v))

  | EConstr (tag, arity) ->
    instantiate_constr tag arity heap env

  | ELet (isrec, defs, body) ->
    if isrec
    then instantiate_letrec defs body heap env
    else instantiate_let defs body heap env

  | ECase _ -> failwith "cant handle case expr"

  | _ -> failwith "dont know how to instantiate"
and instantiate_constr tag arity heap env =
  ti_heap_alloc heap (NPrim ("Pack", (PrimConstr (tag, arity))))
and instantiate_let defs body heap env =
  (* allocate a def in let *)
  let alloc heap (name, rhs) =
    let (h, addr) = instantiate rhs heap env in
    (h, (name, addr)) in
  let (heap', env') = map_accuml alloc heap defs in
  instantiate body heap' (env' @ env)
and instantiate_letrec defs body heap env =
  (* there's probably a better way to do this instead of allocating and copying *)
  (* first allocate all the rhs with dummy values*)
  let dummy = ENum 0 in
  let alloc_dummy heap name =
    let (h, addr) = instantiate dummy heap env in (h, (name, addr)) in
  (* we get a heap' and env' with dummy valus *)
  let (heap', env') = map_accuml alloc_dummy heap (binders_of defs) in
  (* allocate a def in let *)
  let alloc heap (name, rhs) =
    (* we augment the env with dummy env' to allow for letrec rhs *)
    let (h, addr) = instantiate rhs heap (env' @ env) in
    (* find the dummy addr initially allocated, so let's copy it over *)
    let alloc_addr = List.assoc name env' in
    let h = ti_heap_copy h addr alloc_addr in
    (h, (name, alloc_addr)) in
  (* really allocate defs with real rhs *)
  let (heap', env') = map_accuml alloc heap' defs in
  instantiate body heap' (env' @ env)

and instantiate_and_update (expr : core_expr) upd_addr heap env : ti_heap = match expr with
    | EAp (e1, e2) -> let (heap1, a1) = instantiate e1 heap env in
                      let (heap2, a2) = instantiate e2 heap1 env in
                      ti_heap_update heap2 upd_addr (NAp (a1, a2))
    | ENum n -> ti_heap_update heap upd_addr (NNum n)

    | EVar v ->
        let v_addr = List.assoc v env in
        ti_heap_update heap upd_addr (NInd v_addr)

    | EConstr (tag, arity) ->
      ti_heap_update heap upd_addr (NPrim ("Pack", (PrimConstr (tag, arity))))

    | ELet (isrec, defs, body) ->
      if isrec
      then instantiate_letrec_and_update defs body upd_addr heap env
      else instantiate_let_and_update defs body upd_addr heap env

    | ECase _ -> failwith "cant handle case expr"

    | _ -> failwith "dont know how to instantiate"

and instantiate_let_and_update defs body upd_addr heap env : ti_heap =
  (* allocate a def in let *)
  let alloc heap (name, rhs) =
    let (h, addr) = instantiate rhs heap env in
    (h, (name, addr)) in
  let (heap', env') = map_accuml alloc heap defs in
  instantiate_and_update body upd_addr heap' (env' @ env)

and instantiate_letrec_and_update defs body upd_addr heap env =
  (* there's probably a better way to do this instead of allocating and copying *)
  (* first allocate all the rhs with dummy values*)
  let dummy = ENum 0 in
  let alloc_dummy heap name =
    let (h, addr) = instantiate dummy heap env in (h, (name, addr)) in
  (* we get a heap' and env' with dummy valus *)
  let (heap', env') = map_accuml alloc_dummy heap (binders_of defs) in
  (* allocate a def in let *)
  let alloc heap (name, rhs) =
    (* we augment the env with dummy env' to allow for letrec rhs *)
    let upd_addr = List.assoc name env' in
    let h = instantiate_and_update rhs upd_addr heap (env' @ env) in
    (h, (name, upd_addr)) in
  (* really allocate defs with real rhs *)
  let (heap', env') = map_accuml alloc heap' defs in
  instantiate_and_update body upd_addr heap' (env' @ env)


(* a number should never be applied as a function *)
let num_step (stack, dump, heap, globals, stats) n =
  (* rule 2.7, when only item on stack is a number and dump is not empty
   * pop the top of the dump and continue there *)
  match stack, dump with
  | a :: [], s :: d ->
      begin
        match ti_heap_lookup heap a with
        | NNum _ -> (s, d, heap, globals, stats)
        | _ -> failwith "error in num_step"
      end
  | _ -> failwith "Number applied as function"

(* unwind rule *)
let ap_step (stack, dump, heap, globals, stats) a1 a2 =
                (* [a  : NAp  a1 a2] *)
    (* a :: s, d, h[a2 : NInd a3   ], f *)

(* =>  a :: s, d, h[a : NAp a1 a3  ], f *)
  (* rule 2.8, check if arg is an indirection, and update it to the target *)
  match stack, ti_heap_lookup heap a2 with
  | a :: s, NInd a3 -> (stack, dump, ti_heap_update heap a (NAp (a1, a3)), globals, stats)
  | _ -> (a1 :: stack, dump, heap, globals, ti_stat_depth stats (List.length stack + 1))

(* check if an supercombinator is underapplied, fails if so *)
let check_under_application sc args stack_args =
  let n = List.length args in
  let n' = List.length stack_args in
  if n' < n
  then failwith (Format.sprintf "%s called with too little args, %d but needs %d" sc n' n)

(* Combines two lists, forming a new list with length equal to the shorter of the inputs *)
let rec zip2 xs ys = match xs, ys with
  | [], _ | _, [] -> []
  | x::xs, y::ys  -> (x, y)::(zip2 xs ys)

(* get nodes pointed to by addresses on the stack *)
let get_args (_::stack) heap : addr list =
  let rec get_arg addr = match ti_heap_lookup heap addr with
    | NAp (f, arg) -> arg
    | _ -> failwith "error getargs" in
  List.map get_arg stack

let sc_step (stack, dump, heap, globals, stats) sc args body =
  (* get address of all args on the stack, e.g.
   *       (NAp1)
   *       /    \
   *    (NAp2)  (a2)
   *     /  \
   * (NSc)  (a1)
   *
   * NSc takes two arguments, name x and y.
   * The stack contains [NSc; NAp2; NAp1],
   * x needs to be bound to a1, y to a2.
   **)
  (* bind arg names to addresses *)
  let stack_args = get_args stack heap in
  (* check that supercombinator is not underapplied, error out otherwise *)
  check_under_application sc args stack_args;
  (* zip2 will take care when length of args and stack args don't match *)
  let arg_bindings = zip2 args stack_args in
  let env = arg_bindings @ globals in
  (* discard arguments and sc from stack *)
  let (dropped, remaining) = take (List.length args + 1) stack in
  let redex_root = last dropped in
  (* often the redex_root is updated to an NInd to point to new instance *)
  (* rather than the NInd, we can directly update redex_root, saving heap allocs *)
  let (heap') = instantiate_and_update body redex_root heap env in
  let stack' = redex_root :: remaining in
  let is_primitive = List.mem sc prelude_names in
  let count_reduction = if is_primitive then ti_stat_p_r else ti_stat_s_r in
  (* update root of redex with indirection to result *)
  (stack', dump, heap', globals, count_reduction stats)

let prim_neg (stack, dump, heap, globals, stats) =
  (* extract address of arguments from stack and lookup nodes *)
  let arg = match get_args stack heap with
  | arg::[] -> arg
  | _ -> failwith "insufficient args to prim neg"
  in
  let node = ti_heap_lookup heap arg in
  if is_data_node node
  then
    let res =
      begin
        match node with
        | NNum n -> NNum (- n)
        | _ -> failwith "not data node"
      end in
    (* update redex root *)
    begin
      match stack with
      | _ :: a1 :: [] -> ([a1], dump, ti_heap_update heap a1 res , globals, stats)
      | _ -> failwith "stack does not have sufficient"
    end
  (* use rule 2.9 to set up new state to evaluate argument*)
  else
    begin
      match stack with
      | _ :: a1 :: [] ->
        begin
          match ti_heap_lookup heap a1 with
            | NAp (_, b) -> ([b], [a1] :: dump, heap, globals, stats)
            | _          -> failwith "stack can only have NAp"
        end
      | _ -> failwith "stack does not have sufficient"
    end

let prim_dyadic (stack, dump, heap, globals, stats) combine : ti_state =
  (* extract address of arguments from stack and lookup nodes *)
  let (arg1, arg2) = match get_args stack heap with
  | arg1::arg2::[] -> (arg1, arg2)
  | _ -> failwith "insufficient args to prim neg"
  in
  let (node1, node2) = (ti_heap_lookup heap arg1, ti_heap_lookup heap arg2) in
  if is_data_node node1 && is_data_node node2
  then
    let res = combine node1 node2 in
    (* update redex root *)
    begin
      match stack with
      | _ :: _ :: a1 :: [] -> ([a1], dump, ti_heap_update heap a1 res , globals, stats)
      | _ -> failwith "stack does not have sufficient"
    end
  (* use rule 2.9 to set up new state to evaluate argument*)
  else
    begin
      match stack with
      | _ :: a1 :: a2 :: [] ->
        begin
          match ti_heap_lookup heap a1, ti_heap_lookup heap a2 with
            | NAp (_, b), NAp (_, b2) -> ([b], [b2] :: [a2] :: dump, heap, globals, stats)
            | NAp (_, b), _           -> ([b], [a1] :: dump, heap, globals, stats)
            | _, NAp (_, b)           -> ([b], [a2] :: dump, heap, globals, stats)
            | _          -> failwith "stack can only have NAp"
        end
      | _ -> failwith "stack does not have sufficient"
    end

let prim_arith state op =
  let combine node1 node2 =
    match node1, node2 with
    | NNum m, NNum n -> NNum (op m n)
    | _ -> failwith "not num nodes"
  in
  prim_dyadic state combine

let prim_comp state op =
  let combine node1 node2 =
    match node1, node2 with
    | NNum m, NNum n -> let res = op m n in
      if res = true then NData (2, []) else NData (1, [])
    | _ -> failwith "not num nodes"
  in
  prim_dyadic state combine

let prim_add state = prim_arith state (+)
let prim_sub state = prim_arith state (-)
let prim_div state = prim_arith state (/)
let prim_mul state = prim_arith state ( * )
let prim_eq state = prim_comp state (=)
let prim_neq state = prim_comp state (<>)
let prim_gt state = prim_comp state (>)
let prim_gte state = prim_comp state (>=)
let prim_lt state = prim_comp state (<)
let prim_lte state = prim_comp state (<=)

let prim_constr (stack, d, heap, g, st) tag arity =
  (* check that there are enough args *)
  let args = get_args stack heap in
  if List.length args != arity
  then failwith "WRong number of args to build Constr"
  else begin
    let (dropped, _) = take (List.length args + 1) stack in
    let root = last dropped in
    let heap' = ti_heap_update heap root (NData (tag, args)) in
    (root::[], d, heap', g, st)
  end

(* evaluate an if primitive *)
let prim_if (stack, dump, heap, globals, stats) =
  (* ensure we have the test, true clause, false clause on stack *)
  let (test, t, f) = match get_args stack heap with
    | (test::t::f::[]) -> test, t, f
    | _ -> failwith "prim if args mismatch"
  in
  match ti_heap_lookup heap test with
  (* with a NData node, we can jump to the true or false clause *)
  | NData (tf, _) -> begin
      match stack with
      | _ :: _ :: _ :: root :: [] ->
          let target = if tf = 1 then f else t in
          ([target], dump, ti_heap_update heap root (NInd target), globals, stats)
      | _ -> failwith "insufficient args on stack"
    end
  | _ -> begin
      match stack with
      | _ :: a1 :: t :: f :: [] -> begin
          match ti_heap_lookup heap a1 with
          | NAp (_, b) -> ([b], [a1; t; f]::dump, heap, globals, stats)
          | _          -> failwith "stack can only have NAp"
        end
      | _          -> failwith "insufficient args on stack"
    end

let prim_case_pair (stack, dump, heap, globals, stats) =
  (* fst . (mkpair (1) ( 2) *)
  (* CasePair p f *)
  let (p, f) = match get_args stack heap with
    | p::f::[] -> (p, f)
    | _ -> failwith "prim_case_pair argument mismatch"
  in
  match ti_heap_lookup heap p with
  | NData (1, a::b::[]) ->
    begin match stack with
      | _::p::root::[] ->
        (* casepair is translated into function applications *)
        let heap' = ti_heap_update heap p (NAp (f, a)) in
        let heap'' = ti_heap_update heap' root (NAp (p, b)) in
        (* jump to the function given to casepair *)
        ([f; p; root], dump, heap'', globals, stats)
      | _ -> failwith "error!"
    end
  | NData _ -> failwith "unknown data for prim_case_pair"
  | _ -> begin
      match stack with
      | _::p::f::[] -> begin
          match ti_heap_lookup heap p with
          | NAp (_, b) -> ([b], [p; f]::dump, heap, globals, stats)
          | _          -> failwith "stack can only have NAp"
        end
      | _ -> failwith "insufficient args on stack"
    end

let prim_case_list (stack, dump, heap, globals, stats) =
  let (xs, cn, cc) = match get_args stack heap with
    | xs::cn::cc::[] -> (xs, cn, cc)
    | _ -> failwith "prim_case_list argument mismatch"
  in
  match ti_heap_lookup heap xs with
  | NData (1, _) ->
    begin match stack with
      | _::xs::_::root::[] ->
        print_endline ("ndata 1 is " ^ (string_of_int root));
        ([cn], dump, ti_heap_update heap root (NInd cn), globals, stats)
      | _ -> failwith "insufficnet'"
    end
  | NData (2, x_addr::xs_addr::[]) ->
    begin match stack with
      | _::xs::_::root::[] ->
        (* casepair is translated into function applications *)
        print_endline ("ndata 2 is " ^ (string_of_int root)) ;
        let (heap', target) =
          (ti_heap_update
             (ti_heap_update heap xs (NAp (cc, x_addr)))
             root (NAp(xs, xs_addr))), [cc; xs; root]
        in
        (target, dump, heap', globals, stats)
      | _ -> failwith "error!"
    end
  | NData (_, _) -> failwith "Unknown tag for list"
  | _ -> begin
      print_endline "second";
      match stack with
      | _::p::cn::cc::[] -> begin
          match ti_heap_lookup heap p with
          | NAp (_, b) -> ([b], [p; cn; cc]::dump, heap, globals, stats)
          | _          -> failwith "stack can only have NAp"
        end
      | _ -> failwith "insufficient args on stack"
    end

let prim_step state = function
  | Neg -> prim_neg state
  | Add -> prim_add state
  | Sub -> prim_sub state
  | Mul -> prim_mul state
  | Div -> prim_div state
  | PrimConstr (tag, arity) -> prim_constr state tag arity
  | If -> prim_if state
  | Eq -> prim_eq state
  | NotEq -> prim_neq state
  | Greater -> prim_gt state
  | GreaterEq -> prim_gte state
  | Less -> prim_lt state
  | LessEq -> prim_lte state
  | CasePair -> prim_case_pair state
  | CaseList -> prim_case_list state
  | Abort -> failwith "aborted"

let data_step (stack, dump, heap, globals, stats) =
  (* rule 2.7 but for data, when only item on stack is a data and dump is not empty
   * pop the top of the dump and continue there *)
  match stack, dump with
  | a :: [], s :: d ->
      begin
        match ti_heap_lookup heap a with
        | NData _ -> (s, d, heap, globals, stats)
        | _ -> failwith "error in data_step"
      end
  | _ -> failwith "Data applied as function"

(* step to the next state *)
let step (state : ti_state) =
  let (stack, dump, heap, globals, stats) = state in
  (* get node pointed to by addr at top of stack *)
  match ti_heap_lookup heap (List.hd stack) with
    | NAp (a1, a2) -> ap_step state a1 a2
    | NSupercomb (sc, args, body) -> sc_step state sc args body
    | NNum n -> num_step state n
    | NInd a -> (a :: (List.tl stack), dump, heap, globals, stats)
    | NPrim (n, prim) -> prim_step state prim
    | NData (tag, addrs) -> data_step state

(** Facilities for printing the stack *)

(* shows the state of the stack *)
let show_state (stack, _, heap, _, _) =
  (* add a title to a section *)
  let wrap title body = i_concat [ i_str (title ^ "["); i_newline; i_indent body; i_str "]" ] in
  (* show addr in fix width format of 4 *)
  let show_fw_addr addr =
    i_fwnum 4 addr in
  (* show addres *)
  let show_addr addr = i_num addr in
  (* show a node concisely *)
  let show_node node = match node with
    | NAp (a1, a2) -> i_concat [ i_str "NAp "; show_addr a1;
                                i_str " "; show_addr a2 ]
    | NSupercomb (name, args, body) -> i_str ("NSupercomb " ^ name)
    | NNum n -> i_append (i_str "NNum ") (i_num n)
    | NInd a -> i_append (i_str "NInd ") (show_addr a)
    | NPrim (name, prim) -> i_str ("NPrim " ^ name)
    | NData (tag, addrs) ->
      i_concat [ i_str ("NData "); i_num tag; i_str " ";
                 i_interleave (i_str " ") (List.map show_addr addrs) ]
  in
  (* show a node, if it's an application we show the argument *)
  let show_stack_node heap node = match node with
    | NAp (faddr, argaddr) -> i_concat [ i_str "NAp "; show_fw_addr faddr; i_str " ";
                                         show_fw_addr argaddr;
                                         i_str " (";
                                         show_node (ti_heap_lookup heap argaddr);
                                         i_str ")" ]
    | node -> show_node node in
  (* show a node and its address *)
  let show_addr_node addr = i_concat [ show_fw_addr addr; i_str ": ";
                                       show_stack_node heap (ti_heap_lookup heap addr)] in
  (* show the stack *)
  let show_stack = wrap "Stk" (i_line_btwn show_addr_node stack) in
  (* show the heap *)
  let show_heap = wrap "Heap" (i_line_btwn show_addr_node (ti_heap_addrs heap)) in
  i_concat [ show_stack; i_newline;
             show_heap;
             i_newline ]

(* show stats from running machine *)
let show_stats (_, _, (_, n_alloc), _, (steps, p_r, s_r, depth)) =
  i_concat [ i_newline; i_newline;
             i_str "Total number of steps = ";
             i_num (ti_stat_get steps);
             i_newline;
             i_str "Total number of primitive reductions = ";
             i_num p_r;
             i_newline;
             i_str "Total number of supercombinator reductions = ";
             i_num s_r;
             i_newline;
             i_str "Total number of heap allocation = ";
             i_num n_alloc;
             i_newline;
             i_str "Max stack depth = ";
             i_num depth;
           ]

(* show results from running machine *)
let show_results (states : ti_state list) : string =
  let step_title i = i_concat [ i_str "Step "; i_num i; i_newline ] in
  let ss = List.flatten @@ List.mapi
      (fun i s ->
         [step_title i; show_state s]) states in
  (* let ss = List.map show_state states in *)
  let stats = show_stats (last states) in
  i_display (i_concat [ i_layn ss;
                        stats;
                      ])

(* do some admin on state on each step *)
let doAdmin state =
  let state = apply_to_stats ti_stat_inc state in
  print_endline (show_results [state]);
  state
(* let doAdmin state = apply_to_stats ti_stat_inc state *)

let c = ref 0
let rec eval (state : ti_state) : ti_state list =
  let rest_states =
    if ti_final state
    then []
    (* else let _ = c := !c + 1 in if !c > 60 then failwith "over" *)
    else eval (doAdmin (step state))
  in
  state :: rest_states

(* run a Core program in string form *)
let run_prog_string s =
  s |> parse_string |> compile |> eval |> show_results

(* run a Core program in a file *)
let run_prog filename =
  filename |> parse |> compile |> eval |> show_results
