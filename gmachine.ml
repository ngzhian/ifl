open Ast
open Parser
open Util
open Pprint

type instruction =
  | Unwind
  | Pushglobal of name
  | Pushint of int
  | Push of int
  | Mkap
  | Slide of int
type gm_code = instruction list

type addr = int
type gm_stack = addr list

type node =
  | NNum of int
  | NAp of addr * addr
  | NGlobal of int * gm_code

type gm_heap = node list * int

type gm_globals =
  (name * addr) list
  (* association list of name of globals to address in heap *)

type gm_stats = int

type gm_state =
    gm_code    (* current instruction stream *)
  * gm_stack   (* current stack              *)
  * gm_heap    (* heap of nodes              *)
  * gm_globals (* global addresses in heap   *)
  * gm_stats   (* statistics of machine      *)

let get_code (i, _, _, _, _) = i
let put_code i' (i, stack, heap, globals, stats) = (i', stack, heap, globals, stats)

let get_stack (_, stack, _, _, _) = stack
let put_stack stack' (i, stack, heap, globals, stats) = (i, stack', heap, globals, stats)

let get_heap (_, _, heap, _, _) = heap
let put_heap heap' (i, stack, heap, globals, stats) = (i, stack, heap', globals, stats)

(* globals are constant, so no put *)
let get_globals (_, _, _, globals, _) = globals

let stat_initial = 0
let stat_inc_steps s = s + 1
let stat_get_steps s = s

let get_stats (_, _, _, _, stats) = stats
let put_stats stats' (i, stack, heap, globals, stats) = (i, stack, heap, globals, stats')

let pushglobal f state =
  (* look up global in heap *)
  let global =
    try List.assoc f (get_globals state)
    with Not_found -> failwith ("Cannot find global " ^ f) in
  put_stack (global :: get_stack state) state

let pushint n state =
  (* allocate number on heap *)
  let (heap', addr) = Heap.alloc (get_heap state) (NNum n) in
  (* place addr of number onto stack, and update heap *)
  put_heap heap' (put_stack (addr :: get_stack state) state)

let mkap state =
  match get_stack state with
  | a1::a2::stack ->
    (* alloate application node on heap *)
    let (heap', addr) = Heap.alloc (get_heap state) (NAp (a1, a2)) in
    put_heap heap' (put_stack (addr::stack) state)
  | _ -> failwith "not enough nodes to mkapp"

let push n state =
  (* pushes an argument to a function onto the stack *)
  let get_arg = function
    | NAp (a1, a2) -> a2
    | _ -> failwith "getarg error, not NAp"
  in
  let stack = get_stack state in
  let root = List.nth stack (n + 1) in
  let a = get_arg (Heap.lookup (get_heap state) root) in
  put_stack (a::stack) state

let slide n state =
  match get_stack state with
  | a0::stack -> put_stack (a0 :: drop n stack) state
  | _ -> failwith "slide error"

let unwind state =
  match get_stack state with
  | l::ls ->
    let new_state node = match node with
      (* number at the top of the stack means evaluation is done *)
      | NNum _ -> state
      (* application, continue unwinding *)
      | NAp (a1, a2) -> put_code [Unwind] (put_stack (a1::l::ls) state)
      | NGlobal (n, c) ->
        if List.length ls < n
        then failwith "unwinding with too few argus"
        else put_code c state
    in
    new_state (Heap.lookup (get_heap state) l)
  | _ -> failwith "error unwinding"

(* advance by one instruction *)
let step (state : gm_state) : gm_state =
  let (i, is) = match get_code state with
  | [] -> failwith "should not have empty instructions"
  | i::is -> i, is
  in
  let dispatch i = match i with
   | Unwind -> unwind
   | Pushglobal f -> pushglobal f
   | Pushint n -> pushint n
   | Push n -> push n
   | Mkap -> mkap
   | Slide n -> slide n
  in
  (* dispatch on current instruction, update instruction stream to remaining instructions *)
  dispatch i (put_code is state)

let rec eval (state : gm_state) : gm_state list =
  let do_admin state =
    put_stats (stat_inc_steps (get_stats state)) state
  in
  let gm_final state = match get_code state with
    | [] -> true
    | _ -> false
  in
  let rest_states =
    if gm_final state
    then []
    else eval (do_admin (step state))
  in
  state :: rest_states

type gm_compiled_sc = name * int * gm_code
type gm_environment = (name * int) list
type gm_compiler = core_expr -> gm_environment -> gm_code

let compiled_primitives = []

(* change stack offsets *)
let arg_offset n env =
  List.map (fun (v, m) -> (v, m + n)) env

let rec compile_c : gm_compiler = fun e env ->
  match e with
  | EVar v ->
    if (List.mem_assoc v env)
    then [Push (List.assoc v env)]
    else [Pushglobal v]
  | ENum n -> [Pushint n]
  | EAp (e1, e2) -> compile_c e2 env @ (compile_c e1 (arg_offset 1 env)) @ [Mkap]
  | _ -> failwith "HELLO"
  (* | EConstr (_,_) -> (??) *)
  (* | ELet (_,_,_) -> (??) *)
  (* | ECase (_,_) -> (??) *)
  (* | ELam (_,_) -> (??) *)

let compile_r : gm_compiler = fun e env ->
  compile_c e env @ [Slide (List.length env + 1); Unwind]

let compile_sc = function
  | (name, env, body) -> (name, List.length env, compile_r body (indexify env))

let allocate_sc heap compiled_sc : (gm_heap * (name * addr)) = match compiled_sc with
  | name, nargs, instructions ->
    let (heap', addr) = Heap.alloc heap (NGlobal (nargs, instructions)) in
    (heap', (name, addr))

(* TEMP *)
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
  ]

let compile program =
  let build_initial_heap program =
    let compiled = List.map compile_sc (prelude_defs @ program @ compiled_primitives) in
    map_accuml allocate_sc Heap.init compiled
  in
  let (heap, globals) = build_initial_heap program in
  (* entry point is main *)
  let initial_code = [Pushglobal main_entry; Unwind] in
  (initial_code, [], heap, globals, stat_initial)

let show_instruction (i : instruction) = match i with
  | Unwind -> i_str "Unwind"
  | Pushglobal f -> i_append (i_str "Pushglobal ") (i_str f)
  | Pushint n -> i_append (i_str "Pushint ") (i_num n)
  | Push n -> i_append (i_str "Push ") (i_num n)
  | Mkap -> i_str "Mkap"
  | Slide n -> i_append (i_str "Slide ") (i_num n)

let show_instructions is =
  i_concat [
    i_str "  Code:{";
    i_indent (i_interleave i_newline (List.map show_instruction is));
    i_str "}"; i_newline;
  ]

let show_sc state (name, addr) =
  match Heap.lookup (get_heap state) addr  with
  | NGlobal (arity, code) ->
    i_concat [
      i_str "Code for "; i_str name; i_newline;
      show_instructions code; i_newline; i_newline;
    ]
  | _ -> failwith "fail to show sc"

let show_addr = string_of_int
let show_node state addr (node:node) = match node with
  | NNum n -> i_num n
  | NAp (a1, a2) -> i_concat [
      i_str "Ap "; i_str (show_addr a1);
      i_str " "; i_str (show_addr a2);
    ]
  | NGlobal (n, g) ->
    let gs = List.filter (fun (n, a) -> addr = a) (get_globals state) in
    let v = fst (List.hd gs) in
    i_concat [i_str "Global "; i_str v]

let show_stack_item state addr =
  i_concat [
    i_str (show_addr addr); i_str ": ";
    show_node state addr (Heap.lookup (get_heap state) addr);
  ]
let show_stack state =
  i_concat [
    i_str " Stack:[";
    i_indent (i_interleave i_newline
                (* want to show top of stack at the bottom *)
                (List.map (show_stack_item state) (List.rev (get_stack state))));
    i_str "]"
  ]
let show_state state =
  i_concat [
    show_stack state; i_newline;
    show_instructions (get_code state); i_newline;
  ]
let show_stats state =
  i_concat [
    i_str "Steps taken = ";
    i_num (stat_get_steps (get_stats state));
  ]
let show_results states =
  match states with
  | s::ss ->
    i_display (i_concat [
        i_str "Sc definitions"; i_newline;
        i_interleave i_newline (List.map (show_sc s) (get_globals s));
        i_newline; i_newline;
        i_str "State transitions"; i_newline; i_newline;
        i_layn (List.map show_state states);
        i_newline; i_newline;
        show_stats (last states);
      ])
  | _ -> failwith "cannot show results"

(* run a Core program in string form *)
let run_prog_string s =
  s |> parse_string |> compile |> eval |> show_results

