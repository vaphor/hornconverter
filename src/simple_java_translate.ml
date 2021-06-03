(*This file is part of Vaphor

    Vaphor is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Vaphor is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Vaphor.  If not, see <https://www.gnu.org/licenses/>. *)

(* Translation from Java into simple-Java *)
open Java_syntax
open Simple_java_syntax
open Localizing

let hints = ref false

(* Many features of Java are not supported *)
(* When an unsupported feature is encoutered an exception is raised *)
exception Non_supported of string
let non_supported s =
  raise (Non_supported (Printf.sprintf "%s unsupported" s))
let check_support s b =
  if not b then non_supported s

module StringMap = Map.Make( String )


(*Acts like an environment. Should be used to declare variables and retrieve variables*)
let (add_new_var, retrieve_var, reset_vars) = 
  let helper () =
    let map = ref StringMap.empty in
    let reset_vars ()= map := StringMap.empty in
    let add var = try 
                    let _ = StringMap.find var.s_var_name !map in
                    failwith ("Variable with name "^ var.s_var_name ^ " already declared")
                  with 
                  | Not_found -> map := StringMap.add var.s_var_name var !map in
    let get_var name =
      try
        StringMap.find name !map
      with
      | Not_found -> failwith ("Use of undeclared variable in expression : " ^ name) in
    (add, get_var, reset_vars) in
  helper()

(*Converts a java variable name into a simple java variable using retrieve_var*)
let rec tr_var java_name = 
  match java_name with
  | Simple_name(n, ext) -> retrieve_var n
  | Qualified_name(name, qualifier) -> tr_var name
  
let rec tr_unsafe_var java_name = 
  match java_name with
  | Simple_name(n, ext) -> { s_var_name= n;     
           s_var_extent= ext;     
           s_var_type=    St_int;}
  | Qualified_name(name, qualifier) -> tr_unsafe_var name

(*Converts a java type to a simple java type. Arrays are converted to maps and arrays of arrays are converted to maps with a tuple index*)
let rec tr_type = function
  | Boolean -> St_bool
  | Integer t -> St_int
  | Void -> St_void
  | Array_type(t) -> St_array(tr_type t)
  | Iterator_type -> St_int
  | List_type(t) -> St_array(tr_type t)
  | Class_type(Simple_class_type(Simple_name(str, _))) -> non_supported (Printf.sprintf "type class %s" str)
  | _ -> failwith "unknown type"

(*Creates a variable from a java variable*)
let tr_variable (v: variable) =
  { s_var_name     = fst v.var_name ;
    s_var_extent   = snd v.var_name ;
    s_var_type     = tr_type v.var_type ; }

(*Converts unary operators*)
let tr_unary_op = function
  | Not -> Su_not
  | _ -> non_supported "unary operator"

(*Converts binary operators*)
let tr_binary_op = function
  | Or -> Sb_or
  | And -> Sb_and
  | Xor -> Sb_xor
  | Imp -> Sb_imp
  | Bitwise_and | Bitwise_or | Lshift | Rshift | Urshift ->
      non_supported "bitwise operator"
  | Equal -> Sb_eq
  | Lesser_than -> Sb_lt
  | Greater_than -> Sb_gt
  | Lesser_equal -> Sb_le
  | Greater_equal -> Sb_ge
  | Add -> Sb_add
  | Sub -> Sb_sub
  | Mult -> Sb_mul
  | Div -> Sb_div
  | Modulo -> Sb_mod
  | Array_access -> failwith "tr_binary_op:should not happen" 

(*Converts tab[e1][e2][e3] into (tab, e1::e2::e3) *)
(*let rec convert_tree_to_list binary_op = 
  match binary_op with
  |  Binary(Array_access,(tree, ext),e) -> begin match tree with 
                                                 |  Variable(v) -> (v, [e])
                                                 |  o -> let (v, l) = convert_tree_to_list o in (v, l @ [e]) end
  |  _ -> failwith "Unmatched"*)

(*Converts an expression*)
let rec tr_expression (e: expression) =
  match e with
  | Assignment _ -> non_supported "assignment inside expressions"
  | Conditional _ -> non_supported "( e ? e : e )"
  | Binary(Array_access,tab,expr) ->  Se_arrayaccess(tr_expr_e tab, tr_expr_e expr)
  | Binary (b, e0, e1) ->
      Se_binary (tr_binary_op b, tr_expr_e e0, tr_expr_e e1)
  | Unary (u, e0) ->
      Se_unary (tr_unary_op u, tr_expr_e e0)
  | Variable(name) -> (try
     Se_var(tr_var name)
     with _ -> Se_var(tr_unsafe_var name))
  | New_array a -> non_supported "new array inside expression"
  | Integer_constant i -> Se_const (Sc_int i)
  | Float_constant _ -> non_supported "floating-point"
  | Bool_constant b -> Se_const (Sc_bool b)
  | Method_call (m, args) -> (*We only support the call Support.random*)
      begin
        match m with
        | Named_method (Simple_name n) -> non_supported "method call"
        | Named_method (Qualified_name (Simple_name ("Support", _), f)) ->
          begin match f with
          | "random" -> 
               begin
               match args with
                | [ low; high] ->
                    Se_random_bounded ((tr_expr_e low),
				       (tr_expr_e high))
                | [] -> Se_random
                | _ -> failwith "Support.random: arguments"
               end
          | "forall" -> 
            (
              match args with
              | [vname; prop] -> let fvar,ext = tr_expr_e vname in
                                 (match fvar with | Se_var(x) -> Se_forall(x, tr_expr_e prop) | _ -> failwith "forall fist param must be a variable name")
              | _ -> failwith "forall needs 2 arguments"
            )
          | _ -> non_supported "method call 0"
          end
        | Named_method (Qualified_name (Simple_name ("Array", _), f)) ->
            begin match f, args with
            | "size", [tab] -> Se_arraysize(tr_expr_e tab)
            | "size", _ -> failwith "Wrong number of arguments for metho size"
            | "insert", _ -> failwith "insert is not an expression (its a statement)"
            | "remove", _ -> failwith "remove is not an expression (its a statement)"
            | _, _ -> failwith "Unknwon method call"
            end
        | Named_method (Qualified_name (Simple_name ("List", _), f)) ->
            begin match f, args with
            | "size", [tab] -> Se_arraysize(tr_expr_e tab)
            | "size", _ -> failwith "Wrong number of arguments for metho size"
            | "begin", _ -> Se_const (Sc_int(Int64.of_int(0)))
            | "end", [tab] -> Se_arraysize(tr_expr_e tab)
            | "select", [tab; it] -> Se_arrayaccess(tr_expr_e tab, tr_expr_e it)
            | "insert", _ -> failwith "insert is not an expression (its a statement)"
            | "remove", _ -> failwith "remove is not an expression (its a statement)"
            | "next", [it] -> Se_binary(Sb_add, (Se_const (Sc_int(Int64.of_int(1))), snd it), tr_expr_e it)
            | "prev", [it] -> Se_binary(Sb_sub, tr_expr_e it, (Se_const (Sc_int(Int64.of_int(1))), snd it))
            | _, _ -> failwith "Unknwon method call"
            end
        | _ -> non_supported "method call 1"
     end
  | Null -> non_supported "null"
  | Char_constant _ -> non_supported "char"
  | String_constant _ -> non_supported "string"
  | This -> non_supported "this"
  | New _ -> non_supported "new"
  | Field_access _ -> non_supported "field access"
  | Instanceof _ -> non_supported "instance of"

and tr_expr_e (expr, ext) =
  tr_expression expr, ext

(* Helper function for array initialization.
   From (t1::t2::t3, {{{e1, e2}, {e3}}, {{e4, e5, e6}, {e7}}}) returns

   [((0::0::0), e1),
    ((0::0::1), e2),
    ((0::1::0), e3),
    ((1::0::0), e4),
    ((1::0::1), e5),
    ((1::0::2), e6),
    ((1::1::0), e7),
*)
(*let rec get_array_inits tuple init_list extent=
  match (tuple, init_list) with
  | [], Expr_init(e) -> [([], (tr_expr_e e))]
  | [], Array_init(_) -> failwith "Problem in array depth in initialization : initialization array to long for type"
  | a::q, Expr_init(e) -> failwith "Problem in array depth in initialization : type to long for initialization array"
  | a::q, Array_init(l2) -> if a != St_int then failwith "Wrong type in map : should be int";
                            List.flatten (List.mapi (fun i e ->  let todo = get_array_inits q e extent in List.map (fun (key, v) ->  (Se_const(Sc_int(Int64.of_int i)), extent)::key, v) todo) l2)*)


(*Converts initialisations into commands*)
let tr_init (init : var_init) (var : s_var) : s_block =
  match (init, var.s_var_type) with
  | (Expr_init(e), St_array(t)) -> failwith "Can not assign a value to a vector/map in initialization"
  | (Expr_init(e, extent), _ ) -> [Sc_assign((Se_var(var), extent), tr_expr_e (e, extent)), extent]
  | (Tuple_init(tuple), St_array(valuet)) -> (*Printf.eprintf "array inits ignored...";*) []
    (*let rec init_expr e v =
      match e with
      | Tuple_init(l) -> List.mapi (fun i e -> Sc_arrayassign(Se_var(var), Se_const(Sc_int(Int64.of_int i)), e)
      | Expr_init(expr) -> [Sc_assign(var, tr_expr_e (e, extent)), extent]




let assigns = get_array_inits t init var.s_var_extent in
                                                  List.mapi (fun i (keys, value) -> Sc_mapassign(var, (Se_tuple(keys), var.s_var_extent) , value), 
                                                                                                 {extent_beg=var.s_var_extent.extent_beg;
                                                                                                  extent_end=var.s_var_extent.extent_end;
                                                                                                  extent_unique_id = var.s_var_extent.extent_unique_id +i;} ) assigns*)
  (*| (Tuple_init(l), St_array(t)) -> failwith "Only map with tuple access allowed"*)
  | (Tuple_init(l), _) -> failwith "Trying to initialize a non map variable with an array"


(*Converts statement*)
let rec tr_statement_e ((s, ext): statement_e): s_block =
  match s with
  | Hint(e) -> if !hints then [Sc_assert (tr_expr_e e), ext] else []
  | Local_var vd ->
      non_supported "local variable declaration"
  | If_then (e, s) ->
      [ Sc_if (tr_expr_e e, tr_block [s], []), ext ]
  | If_then_else (e, s0, s1) ->
      [ Sc_if (tr_expr_e e, tr_block [ s0 ], tr_block [ s1 ]),
        ext ]
  | While (e, s0) ->
      [ Sc_while (tr_expr_e e, tr_block [ s0 ]), ext ]
  | Expression (Method_call (name, args), _) ->
      begin
        match name with
        | Named_method (Qualified_name (Simple_name ("Support", _), f)) ->
          (
            match f with
            | "assume" ->
              begin
              match args with
                | [e] -> [ Sc_assume(tr_expr_e e), ext]
                | _ -> failwith "Support.assume wrong # of arguments" 
              end
            | "invariant" ->
              begin
              match args with
                | [e] -> [Sc_invariant(tr_expr_e e), ext]
                | _ -> failwith "Support.invariant wrong # of arguments" 
              end
            | _ -> non_supported "method call 3"
          )
        | Named_method (Qualified_name (Simple_name ("Array", _), f)) -> 
            begin match (f, args) with
            | "insert", tab::index::value::[] -> [Sc_arrayinsert(tr_expr_e tab, tr_expr_e index, tr_expr_e value), ext]
            | "insert", _ -> failwith "Wrong number of arguments for insert statement"
            | "remove", tab::index::[] -> [Sc_arrayremove(tr_expr_e tab, tr_expr_e index), ext]
            | "remove", _ -> failwith "Wrong number of arguments for remove statement"
            | "clear", [tab] -> [Sc_arrayclear(tr_expr_e tab), ext]
            | "clear", _ -> failwith "Wrong number of arguments for clear statement"
            | _, _ -> failwith "Unknwon array method"
            end
        | Named_method (Qualified_name (Simple_name ("List", _), f)) -> 
            begin match (f, args) with
            | "insert", tab::index::value::[] -> [Sc_arrayinsert(tr_expr_e tab, tr_expr_e index, tr_expr_e value), ext]
            | "insert", _ -> failwith "Wrong number of arguments for insert statement"
            | "remove", tab::index::[] -> [Sc_arrayremove(tr_expr_e tab, tr_expr_e index), ext]
            | "remove", _ -> failwith "Wrong number of arguments for remove statement"
            | "clear", [tab] -> [Sc_arrayclear(tr_expr_e tab), ext]
            | "clear", _ -> failwith "Wrong number of arguments for clear statement"
            | _, _ -> failwith "Unknwon list method"
            end
        | _ -> non_supported "method call 4"
      end
  | Expression (Assignment assign, _) ->
     begin
	match assign with
	| (Variable v, _), None, e ->
	   [ Sc_assign (tr_expr_e (Variable v, ext), tr_expr_e e), ext ]
        |  (Binary(Array_access, tab, e), s),None,eres -> [Sc_arrayassign(tr_expr_e tab, tr_expr_e e, tr_expr_e eres), ext] (*let (v, l) = convert_tree_to_list (Binary(Array_access,(tree, ex),e)) in 
                                                                  [ Sc_mapassign (tr_var v, (Se_tuple((List.map (fun e -> tr_expr_e e) l)), ext), tr_expr_e eres), ext ]*)
	| _ -> non_supported "complex assignment"
      end
  | Expression _ -> non_supported "non-assignment expression"
  | Block_statement b -> (*was : non_supported "block"*)
     List.rev (tr_block b)
  | Return None -> non_supported "return"
  | Return (Some e) -> non_supported "return"
  | Assert (e, None) -> 
      [ Sc_assert (tr_expr_e e), ext ]
  | Assert (_, _) -> non_supported "assert(e,e)"
  | Type_decl _ -> non_supported "type declaration"
  | Labeled _ -> non_supported "labeled statement"
  | For _ -> non_supported "for statement"
  | Foreach _ -> non_supported "for each"
  | Do_while _ -> non_supported "do while"
  | Empty_statement -> non_supported "empty statement"
  | Switch _ -> non_supported "switch statement"
  | Break _ -> non_supported "break statement"
  | Continue _ -> non_supported "continue statement"
  | Throw _ -> non_supported "throw execption"
  | Synchronized_statement _ -> non_supported "synchronized"
  | Try _ -> non_supported "try statement"

and tr_block (b: block) =
  let sb =
    List.fold_left
      (fun b_acc st ->
        let stat = tr_statement_e st in
        stat @ b_acc
      ) [ ] b in
  List.rev sb



(*Converts a function declaration*)
let tr_method_declaration (m: method_declaration) =
  check_support "non static methods" (List.mem Static m.method_modifiers);
  check_support "method modifiers" (List.length m.method_modifiers = 1);
  check_support "method exceptions" (m.method_throws = [ ]);
  check_support "formal parameters" (m.method_parameters = [ ]);
  if m.method_type != Void then non_supported "non void type function";
  { s_proc_name   = fst m.method_name ;
    s_proc_body   = tr_block m.method_body ; }



let tr_java_prog (jp: java_prog): s_program =
  check_support "packages" (jp.package = None);
  check_support "import" (jp.import = []);

  List.map (fun c_decl -> match c_decl with
                          |  Class_declaration(c) -> 
                               (*Reseting declared variables*)
                               reset_vars();
                               if c.class_modifiers != [] then non_supported "Class Modifiers";
                               if c.class_inherits != None then non_supported "Class Inheritance";
                               if c.class_interfaces != [] then non_supported "Class Interfaces";
                               (*initialize is a new function created from the initializations*)
                               let (variables, functions, initialize) = List.fold_left (
                                 fun (variables, functions, initialize) decl -> match decl with 
                                 |  Class_member(member) -> begin match member with
                                                           |  Field_decl(var) -> add_new_var (tr_variable var); begin match var.var_initializer with
                                                                               |  None -> ((tr_variable var)::variables, functions, initialize)
                                                                               |  Some(init) -> ((tr_variable var)::variables, functions, (tr_init init (tr_variable var)) @ initialize) end
                                                           |  Method_decl(proc) -> (variables, (tr_method_declaration proc)::functions, initialize)
                                                           |  Class_decl(c) -> non_supported "Class within classes"
                                                           |  Enum_decl(e) -> non_supported "Enums within classes"
                                                           |  Interface_decl(i) -> non_supported "Interfaces within classes" end
                                 |  Static_init(_) -> non_supported "Static Init"
                                 |  Constructor_decl(_) -> non_supported "Constructors"
                                 |  Block(b) -> non_supported "Blocks in class"
                                 ) ([], [], []) c.class_body in
                               let init_func =
                               {
                                 s_proc_name = "_initialize";
                                 s_proc_body = initialize;
                               } in
                               {
                                  s_class_name = fst c.class_name;
                                  s_class_variables = variables;
                                  s_class_functions = init_func::functions;
                               }
                           |  Enum_declaration(e) -> non_supported "Enums"
                           |  Interface_declaration(c) -> non_supported "Interfaces") jp.type_decls

