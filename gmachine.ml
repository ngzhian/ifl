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
  | Update of int
  | Pop of int
  | Alloc of int
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond of gm_code * gm_code
  | Pack of int * int
  | Casejump of (int * gm_code) list
  | Split of int
  | Print
and gm_code = instruction list

type addr = int
type gm_stack = addr list

type gm_dump_item = gm_code * gm_stack
type gm_dump = gm_dump_item list

type node =
  | NNum of int
  | NAp of addr * addr
  | NGlobal of int * gm_code
  | NInd of addr
  | NConstr of int * addr list (* tag and components *)

type gm_heap = node list * int

type gm_globals =
  (name * addr) list
  (* association list of name of globals to address in heap *)

type gm_stats = int

type gm_output = string

type gm_state =
    gm_output  (* current output *)
  * gm_code    (* current instruction stream *)
  * gm_stack   (* current stack              *)
  * gm_dump    (* current dump               *)
  * gm_heap    (* heap of nodes              *)
  * gm_globals (* global addresses in heap   *)
  * gm_stats   (* statistics of machine      *)

let get_output (output, _, _, _, _, _, _) : gm_output = output
let put_output output' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output', i, stack, dump, heap, globals, stats)

let get_code (_, i, _, _, _, _, _) : gm_code = i
let put_code i' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output, i', stack, dump, heap, globals, stats)

let get_stack (_, _, stack, _, _, _, _) : gm_stack = stack
let put_stack stack' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output, i, stack', dump, heap, globals, stats)

let get_dump (_, _, _, dump, _, _, _) :gm_dump = dump
let put_dump dump' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output, i, stack, dump', heap, globals, stats)

let get_heap (_, _, _, _, heap, _, _) : gm_heap = heap
let put_heap heap' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output, i, stack, dump, heap', globals, stats)

let get_globals (_, _, _, _, _, globals, _) : gm_globals = globals
let put_globals globals' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output, i, stack, dump, heap, globals', stats)

let stat_initial = 0
let stat_inc_steps s = s + 1
let stat_get_steps s = s

let get_stats (_, _, _, _, _, _, stats) : gm_stats = stats
let put_stats stats' (output, i, stack, dump, heap, globals, stats) : gm_state =
  (output, i, stack, dump, heap, globals, stats')

let pushglobal f state =
  (* look up global in heap *)
  let global =
    try List.assoc f (get_globals state)
    with Not_found -> failwith ("Cannot find global " ^ f) in
  put_stack (global :: get_stack state) state

let pushint n state =
  (* optimization, treat number as globals, so can be reused *)
  let n_str = string_of_int n in
    try let global = List.assoc n_str (get_globals state) in
      (* if there is a global, reuse it *)
      put_stack (global :: get_stack state) state
    with Not_found ->
      (* otherwise, create and put onto the heap for reuse *)
      let (heap', addr) = Heap.alloc (get_heap state) (NNum n) in
      (* place addr of number onto stack, and update heap and globals *)
      let globals' = (n_str, addr) :: get_globals state in
      let stack' = (addr::get_stack state) in
      put_globals globals' (put_heap heap' (put_stack stack' state))

let mkap state =
  match get_stack state with
  | a1::a2::stack ->
    (* alloate application node on heap *)
    let (heap', addr) = Heap.alloc (get_heap state) (NAp (a1, a2)) in
    put_heap heap' (put_stack (addr::stack) state)
  | _ -> failwith "not enough nodes to mkapp"

let push n state =
  (* pushes an argument to a function onto the stack *)
  (* let get_arg = function *)
  (*   | NAp (a1, a2) -> a2 *)
  (*   | _ -> failwith "getarg error, not NAp" *)
  (* in *)
  let stack = get_stack state in
  let an = List.nth stack n in
  (* let a = get_arg (Heap.lookup (get_heap state) an) in *)
  put_stack (an::stack) state

let slide n state =
  match get_stack state with
  | a0::stack -> put_stack (a0 :: drop n stack) state
  | _ -> failwith "slide error"

let unwind state =
  let get_arg = function
    | NAp (a1, a2) -> a2
    | _ -> failwith "getarg error, not NAp"
  in
  (* rearranges the stack so that we put all the args directly onto the stack  *)
  let rearrange n heap addrs =
    let ap_addrs = List.tl addrs in
    let addrs' = List.map (fun addr -> get_arg (Heap.lookup heap addr)) ap_addrs in
    fst (take n addrs') @ drop n addrs
  in
  match get_stack state with
  | l::ls ->
    begin
      match Heap.lookup (get_heap state) l with
      (* number at the top of the stack means evaluation is done *)
      | NNum _ | NConstr _ ->
        begin
          match get_dump state with
          | [] -> failwith "shouldn't reach this point"
          | (code, stack)::dump ->
            put_code code (put_stack (l::stack) (put_dump dump state))
        end
      (* application, continue unwinding *)
      | NAp (a1, a2) -> put_code [Unwind] (put_stack (a1::l::ls) state)
      | NGlobal (n, c) ->
        if List.length ls < n
        then
          begin
            match get_dump state with
            | (i, s)::d ->
              let ak = last ls in
              put_code i (put_stack (ak :: s) (put_dump d state))
            | _ -> failwith "cannot unwind global"
          end
          (* failwith "unwinding with too few argus" *)
        else
          let stack' = rearrange n (get_heap state) (get_stack state) in
          put_stack stack' (put_code c state)
      | NInd a ->
        put_code [Unwind] (put_stack (a::ls) state)
    end
  | _ -> failwith "error unwinding"

let update n state =
  match get_stack state with
  | a :: stack ->
    let f_and_args, remains = take (n + 1) stack in
    let an = last f_and_args in
    let heap' = Heap.update (get_heap state) an (NInd a) in
    put_stack stack (put_heap heap' state)
  | _ -> failwith "error update"

let pop n state =
  let stack = drop n (get_stack state) in
  put_stack stack state

let alloc n state =
  let rec alloc_nodes n heap : (gm_heap * addr list) = match n with
    | 0 -> heap, []
    | n -> let heap', addrs = alloc_nodes (n - 1) heap in
           let heap'', a = Heap.alloc heap' (NInd Heap.null)
      in (heap'', a :: addrs)
  in
  let heap, addrs = alloc_nodes n (get_heap state) in
  let stack' = addrs @ get_stack state in
  put_stack stack' (put_heap heap state)

let eval (state :gm_state) : gm_state =
  match get_stack state with
  | a::stack ->
    let dump = (get_code state, stack)::get_dump state in
    put_code [Unwind] (put_stack [a] (put_dump dump state))
  | _ -> failwith "error eval"

let box_integer n state : gm_state =
  let h', a = Heap.alloc (get_heap state) (NNum n) in
  put_stack (a::get_stack state) (put_heap h' state)

let unbox_integer addr state : int =
  match Heap.lookup (get_heap state) addr with
  | NNum n -> n
  | _ -> failwith "unboxing non-int"

let primitive1 box unbox op state =
  match get_stack state with
  | s::ss -> box (op (unbox s state)) (put_stack ss state)
  | _ -> failwith "primitive1 error"

let primitive2 box unbox op state =
  match get_stack state with
  | s0::s1::ss -> box (op (unbox s0 state) (unbox s1 state)) (put_stack ss state)
  | _ -> failwith "primitive1 error"

let arithmetic1 = primitive1 box_integer unbox_integer
let arithmetic2 = primitive2 box_integer unbox_integer

let box_boolean b state =
  let b' = if b then 2 (* true *) else 1 (* false *) in
  let h', a = Heap.alloc (get_heap state) (NConstr (b', [])) in
  put_stack (a::get_stack state) (put_heap h' state)

let comparison =
  primitive2 box_boolean unbox_integer

let cond i1 i2 state : gm_state =
  match get_stack state with
  | a::s -> begin
      match Heap.lookup (get_heap state) a with
      | NNum 1 -> put_code (i1 @ get_code state) (put_stack s state)
      | NNum 0 -> put_code (i2 @ get_code state) (put_stack s state)
      | _ -> failwith "error: top of stack not num"
    end
  | _ -> failwith "error: no sufficient on stack"

let pack tag arity state : gm_state =
  let args, rem = take arity (get_stack state) in
  let node = NConstr (tag, args) in
  let heap', addr = Heap.alloc (get_heap state) node in
  put_stack (addr :: rem) (put_heap heap' state)

let casejump alts state : gm_state =
  match get_stack state with
  | a::s -> begin
      match Heap.lookup (get_heap state) a with
      | NConstr (t, ss) ->
        let i' = List.assoc t alts in
        let code' = i' @ (get_code state) in
        put_code code' state
      | _ -> failwith "casejump stack top not in WHNF"
    end
  | _ -> failwith "error: no sufficient on stack"

let split n state : gm_state =
  match get_stack state with
  | a::s -> begin
      match Heap.lookup (get_heap state) a with
      | NConstr (t, ss) ->
        let stack' = ss @ s in
        put_stack stack' state
      | _ -> failwith "split stack top not in WHNF"
    end
  | _ -> failwith "error: no sufficient on stack"

let print state : gm_state =
  match get_stack state with
  | a::s -> begin
      match Heap.lookup (get_heap state) a with
      | NNum n ->
        put_stack s (put_output ((get_output state) ^ (string_of_int n)) state)
      | NConstr (t, ss) ->
        let code' = List.fold_left (fun a b -> Eval::Print::a) [] ss in
        put_code code' (put_stack (ss @ s) state)
      | _ -> failwith "error: cannot print"
    end
  | _ -> failwith "error: no sufficient on stack"


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
   | Update n -> update n
   | Pop n -> pop n
   | Alloc n -> alloc n
   | Eval -> eval
   | Add -> arithmetic2 (+)
   | Sub -> arithmetic2 (-)
   | Mul -> arithmetic2 ( * )
   | Div -> arithmetic2 (/)
   | Neg -> arithmetic1 (fun n -> -n)
   | Cond (i1, i2) -> cond i1 i2
   | Eq -> comparison (=)
   | Ne -> comparison (<>)
   | Lt -> comparison (>)
   | Le -> comparison (>=)
   | Gt -> comparison (<)
   | Ge -> comparison (<=)
   | Pack (t, n) -> pack t n
   | Casejump alts -> casejump alts
   | Split n -> split n
   | Print -> fun x -> x
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

let compiled_primitives : gm_compiled_sc list = [
    ("+", 2, [Push 1; Eval; Push 1; Eval; Add; Update 2; Pop 2; Unwind]);
    ("-", 2, [Push 1; Eval; Push 1; Eval; Sub; Update 2; Pop 2; Unwind]);
    ("*", 2, [Push 1; Eval; Push 1; Eval; Mul; Update 2; Pop 2; Unwind]);
    ("/", 2, [Push 1; Eval; Push 1; Eval; Div; Update 2; Pop 2; Unwind]);
    ("negate", 1, [Push 0; Eval; Neg; Update 1; Pop 1; Unwind]);
    ("==", 2, [Push 1; Eval; Push 1; Eval; Eq; Update 2; Pop 2; Unwind]);
    ("~=", 2, [Push 1; Eval; Push 1; Eval; Ne; Update 2; Pop 2; Unwind]);
    ("<", 2, [Push 1; Eval; Push 1; Eval; Lt; Update 2; Pop 2; Unwind]);
    ("<=", 2, [Push 1; Eval; Push 1; Eval; Le; Update 2; Pop 2; Unwind]);
    (">", 2, [Push 1; Eval; Push 1; Eval; Gt; Update 2; Pop 2; Unwind]);
    (">=", 2, [Push 1; Eval; Push 1; Eval; Ge; Update 2; Pop 2; Unwind]);
    (* ("if", 3, [Push 0; Eval; Cond ([Push 1], [Push 2]); Update 3; Pop 3; Unwind]); *)
]

(* change stack offsets *)
let arg_offset n env : gm_environment =
  List.map (fun (v, m) -> (v, m + n)) env

(* d scheme *)
let compile_alts (comp : int -> gm_compiler) alts env =
  let c (tag, names, body) =
    (tag, comp (List.length names) body (indexify names @ arg_offset (List.length names) env)) in
  List.map c alts

let rec compile_let' comp defs env : gm_code = match defs with
  | [] -> []
  | (name, expr)::defs ->
    comp expr env @  (compile_let' comp defs (arg_offset 1 env))

let compile_args defs env : gm_environment =
  let n = List.length defs in
  List.mapi (fun i (name, expr) -> (name, n-i-1)) defs
    @ arg_offset n env

let compile_let compile_c compile_e defs expr env =
  let env' = compile_args defs env in
  compile_let' compile_c defs env @ (compile_e expr env' @ [Slide (List.length defs)])

let rec compile_letrec' comp defs env : gm_code = match defs with
  | [] -> []
  | (name, expr)::defs ->
    comp expr env @ [Update (List.length defs)] @ (compile_letrec' comp defs (arg_offset 1 env))

let compile_letrec compile_c compile_e defs expr env =
  let n = List.length env in
  let env' = compile_args defs env in
  [Alloc n] @ compile_letrec' compile_c defs env' @ compile_e expr env' @ [Slide n]

let built_in_dyadic = [
  ("+", Add); ("-", Sub); ("*", Mul); ("/", Div);
  ("==", Eq); ("~=", Ne); (">=", Ge); (">", Gt); ("<=", Le); ("<", Lt)
]

let rec compile_e : gm_compiler = fun e env ->
  match e with
  | ENum n -> [Pushint n]
  | ELet (recursive, defs, e)  ->
    if recursive
    then compile_letrec compile_c compile_e defs e env
    else compile_let compile_c compile_e defs e env
  | EAp (EAp (EVar f, e0), e1) when List.mem_assoc f built_in_dyadic ->
    compile_e e0 env @ compile_e e1 (arg_offset 1 env) @ [List.assoc f built_in_dyadic]
  | EAp (EVar "negate", e) -> compile_e e env @ [Neg]
  (* | EAp (EAp (EAp (EVar "if", e0), e1), e2) -> *)
  (*   compile_e e0 env @ [Cond (compile_e e1 env, compile_e e2 env)] *)
  | ECase (expr, alts) -> compile_e expr env @ [Casejump (compile_alts compile_e' alts env)]
  | EConstr (tag, arity) -> [Pack (tag, arity)]
  | e -> compile_c e env @ [Eval]

and compile_e' offset : gm_compiler = fun e env ->
  [Split offset] @ compile_e e env @ [Slide offset]

and compile_c : gm_compiler = fun e env ->
  match e with
  | EVar v ->
    if (List.mem_assoc v env)
    then [Push (List.assoc v env)]
    else [Pushglobal v]
  | ENum n -> [Pushint n]
  | EAp (e1, e2) -> compile_c e2 env @ (compile_c e1 (arg_offset 1 env)) @ [Mkap]
  | EConstr (tag, arity) -> [Pack (tag, arity)]
  | ELet (recursive, defs, e)  ->
    if recursive
    then compile_letrec compile_c compile_e defs e env
    else compile_let compile_c compile_c defs e env
  (* | ECase (_,_) -> (??) *)
  (* | ELam (_,_) -> (??) *)
  | _ -> failwith "HELLO"

let compile_r : gm_compiler = fun e env ->
  (* compile_c e env @ [Slide (List.length env + 1); Unwind] *)
  let n = List.length env in
  compile_e e env @ [Update n; Pop n; Unwind]

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

    ("if", ["c"; "t"; "f"], ECase (EVar "c", [(1, [], EVar "f"); (2, [], EVar "t")]))
  ]

let compile program : gm_state =
  let build_initial_heap program =
    let compiled = List.map compile_sc (prelude_defs @ program) @ compiled_primitives in
    map_accuml allocate_sc Heap.init compiled
  in
  let (heap, globals) = build_initial_heap program in
  (* entry point is main *)
  let initial_code = [Pushglobal main_entry; Eval; Print] in
  ("", initial_code, [], [], heap, globals, stat_initial)

let rec show_instruction (i : instruction) = match i with
  | Unwind -> i_str "Unwind"
  | Pushglobal f -> i_append (i_str "Pushglobal ") (i_str f)
  | Pushint n -> i_append (i_str "Pushint ") (i_num n)
  | Push n -> i_append (i_str "Push ") (i_num n)
  | Mkap -> i_str "Mkap"
  | Slide n -> i_append (i_str "Slide ") (i_num n)
  | Update n -> i_append (i_str "Update ") (i_num n)
  | Pop n -> i_append (i_str "Pop ") (i_num n)
  | Alloc n -> i_append (i_str "Alloc ") (i_num n)
  | Eval -> i_str "Eval"
  | Add -> i_str "Add"
  | Sub -> i_str "Sub"
  | Mul -> i_str "Mul"
  | Div -> i_str "Div"
  | Neg -> i_str "Neg"
  | Cond (i1, i2) -> i_concat [
      i_str "Cond"
    ]
  | Eq -> i_str "Eq"
  | Ne -> i_str "Ne"
  | Lt -> i_str "Lt"
  | Le -> i_str "Le"
  | Gt -> i_str "Gt"
  | Ge -> i_str "Ge"
  | Pack (tag, arity) -> i_concat [
      i_str "Pack{" ; i_num tag; i_str ","; i_num arity; i_str "}"; ]
  | Casejump alts ->
    let print_alt (tag, code) =
      i_concat [ i_num tag; i_str " -> "; i_indent (show_instructions code); ] in
    i_interleave (i_str " ; ") (List.map print_alt alts)
    (* of (int * gm_code) list *)
  | Split n -> i_append (i_str "Split ") (i_num n)
  | Print -> i_str "Print"

and show_instructions is =
  i_concat [
    i_str "  Code:{"; i_newline;
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

let show_addr addr = i_str (string_of_int addr)
let show_node state addr (node:node) = match node with
  | NNum n -> i_num n
  | NAp (a1, a2) -> i_concat [
      i_str "Ap "; show_addr a1;
      i_str " "; show_addr a2;
    ]
  | NGlobal (n, g) ->
    let gs = List.filter (fun (n, a) -> addr = a) (get_globals state) in
    let v = fst (List.hd gs) in
    i_concat [i_str "Global "; i_str v]
  | NInd addr -> i_concat [i_str "Ind "; show_addr addr]
  | NConstr (t, addrs) ->
    i_concat [
      i_str "Cons "; i_num t; i_str " [";
      i_interleave (i_str ", ") (List.map show_addr addrs); i_str "]";
    ]

let show_stack_item state addr =
  i_concat [
    show_addr addr; i_str ": ";
    show_node state addr (Heap.lookup (get_heap state) addr);
  ]
let show_stack state =
  i_concat [
    i_str "  Stack:["; i_newline;
    i_indent (i_interleave i_newline
                (* want to show top of stack at the bottom *)
                (List.map (show_stack_item state) (List.rev (get_stack state))));
    i_str "]"
  ]
let show_short_instructions number code =
  let codes = List.map show_instruction (fst (take number code)) in
  let dotcodes = if List.length code > number
    then codes @ [i_str "..."]
    else codes
  in
  i_concat [ i_str "{"; i_interleave (i_str "; ") dotcodes; i_str "}"]
let show_short_stack stack =
  i_concat [ i_str "[";
             i_interleave (i_str ", ") (List.map (fun addr -> show_addr addr) stack);
             i_str "]"
           ]
let show_dump_item (code, stack) =
  i_concat [ i_str "<";
             show_short_instructions 3 code; i_str ", ";
             show_short_stack stack; i_str ">" ]
let show_dump (state:gm_state) =
  i_concat [
    i_str "  Dump:["; i_newline;
    i_indent (i_interleave i_newline
                (* want to show top of stack at the bottom *)
                (List.map show_dump_item (List.rev (get_dump state))));
    i_str "]"
  ]
let show_output (state : gm_state) =
  i_concat [ i_str "Output:\""; i_str (get_output state); i_str "\""]

let show_state state =
  i_concat [
    show_output state; i_newline;
    show_stack state; i_newline;
    show_dump state; i_newline;
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
