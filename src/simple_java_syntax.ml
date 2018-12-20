(* This file defines the Abstract Syntax Tree for "Simplified Java", *)
(* a minimalist subset of Java.                                      *)
(* We define only data-types in this file; functions modifying       *)
(* these will appear in separate files.                              *)

(* This module allows to track positions in the source files.   *)
open Localizing

(* Keys (to associate to variables). *)
type s_uniqueId = int
  
(* Data-types: we only support int, bool, void, maps and partially tuples *)
type s_type =
  | St_int
  | St_bool
  | St_void
  | St_array of s_type

(* A variable *)
and s_var =
    { s_var_name:     string;     (* name *)
      s_var_extent:   extent;     (* position in the source file *)
      s_var_type:     s_type;     (* type *)
    }

  

let rec type2string = function
  | St_int -> "int"
  | St_bool -> "bool"
  | St_void  -> "void"
  | St_array(value) -> Printf.sprintf "Array %s" (type2string value)
      
(* Constants. *)
type s_constant =
  | Sc_int of int64
  | Sc_bool of bool

let constant2string = function
  | Sc_int(v64) -> Printf.sprintf "%Ld" v64 
  | Sc_bool(b) -> if b then "true" else "false"
		 
(* Operators. *)
(* Unary operators. *)
type s_unary_op =
  | Su_neg
  | Su_not

let uop2string = function
  | Su_neg -> "-"
  | Su_not -> "!"
    
(* Binary operators. *)
type s_binary_op =
  | Sb_add | Sb_sub | Sb_mul | Sb_div | Sb_mod
  | Sb_or | Sb_and | Sb_xor
  | Sb_lt | Sb_le | Sb_gt | Sb_ge
  | Sb_eq
      
let bop2string = function
  | Sb_add -> "+"
  | Sb_sub -> "-"
  | Sb_mul -> "*"
  | Sb_div -> "/"
  | Sb_mod -> "%"
  | Sb_or -> "||"
  | Sb_and -> "&&"
  | Sb_xor -> "^"
  | Sb_lt -> "<"
  | Sb_le -> "<="
  | Sb_gt -> ">"
  | Sb_ge -> ">="
  | Sb_eq -> "=="


(* A procedure, with no parameter:
 *  - it should not return a value
 *  - it should not take any parameter *)
type s_proc =
    { s_proc_name:   string;     (* name *)
      s_proc_body:   s_block;    (* code of the function *) }

(* A function call *)
and  s_proc_call =
    { s_proc_call_class:  string;        (* class of the function called *)
      s_proc_call_name:   string;        (* its name *) }
      
(* Arithmetic and boolean expressions + intarrays + arrays*)
and s_expr =
  | Se_const of s_constant
  | Se_var of s_var
  | Se_unary of s_unary_op * s_expr_e
  | Se_binary of s_binary_op * s_expr_e * s_expr_e
  | Se_arrayaccess of s_expr_e * s_expr_e (* tab[s_expr_e]*)
  | Se_arraysize of s_expr_e (*tab.size()*)
  | Se_random_bounded of s_expr_e * s_expr_e
  | Se_random
  | H_store of s_expr_e * s_expr_e * s_expr_e
					    
(* Expressions annotated with a position in the source file *)
and s_expr_e = s_expr * extent
			  
(* Instructions (assignment, if, while, etc... *)
and s_command =
  | Sc_assign of s_expr_e * s_expr_e (* s_var = s_expr_e *)
  | Sc_arrayassign of s_expr_e * s_expr_e * s_expr_e (* s_var[s_expr_e]=s_expr_e. We will force the index to be a tuple for normalization issues *)
  (*| Sc_random_assign of s_expr_e * s_expr_e * s_expr_e*)
  | Sc_if of s_expr_e * s_block * s_block (* if s_expr_e then s_block else s_block *)
  | Sc_while of s_expr_e * s_block (* while s_expr_e do s_block *)
  | Sc_proc_call of s_proc_call * s_expr_e (*proc(expression)*)
  | Sc_assert of s_expr_e (* assert(s_expr_e) *)
  | Sc_assume of s_expr_e (* assume(s_expr_e) *)
  | Sc_arrayinsert of s_expr_e * s_expr_e * s_expr_e (*tab.insert(i, e)*)
  | Sc_arrayremove of s_expr_e * s_expr_e (*tab.remove(i, e)*)
  | Sc_arrayclear of s_expr_e
		   
(* Instructions annotated with a position in the source file. *)
and s_command_e = s_command * extent

(* A block is a sequence of instructions. *)
and s_block = s_command_e list

(* A class = a name + a sequence of declarations *)
type s_class =
    { s_class_name: string;
      s_class_variables: s_var list;
      s_class_functions: s_proc list; }

(* A program = a list of classes *)
type s_program = s_class list



(*warning only print expression, not location info*)
let rec sexpr2string se = match se with
  | Se_const(sct) -> constant2string sct
  (*| Se_random_bounded((lb, _), (ub, _)) -> Printf.sprintf "rand[%s, %s]"
       (sexpr2string lb) (sexpr2string ub)
  | Se_random -> Printf.sprintf "rand"*)
  | Se_var(svar) -> svar.s_var_name
  | Se_unary(uop,se1) -> "("^(uop2string uop)^(sexpr2string (fst se1))^")"
  | Se_binary(bop,se1,se2) -> "("^(sexpr2string (fst se1))^(bop2string bop)^(sexpr2string (fst se2))^")"
  | Se_arrayaccess(tab,key) -> Printf.sprintf "%s[%s]" (sexpr2string (fst tab)) (sexpr2string (fst key))
  | Se_arraysize(tab) -> Printf.sprintf "%s.size()" (sexpr2string (fst tab))
  | Se_random -> Printf.sprintf "Support.random()"
  | Se_random_bounded(low, high) -> Printf.sprintf "Support.random(%s, %s)" (sexpr2string (fst low)) (sexpr2string (fst high))

(* Avar = s_var * option (s_expr_e) where the option is the initialization*)			 
let pp_var_decl avar =
  let vname,other = avar in
  let str = Printf.sprintf "var decl : name =%s, type=%s, pos=%s" vname.s_var_name
		(type2string vname.s_var_type)
		(Localizing.extent_to_string vname.s_var_extent) in
  match other with (*initialization*)
  | None -> str ^ Printf.sprintf "\n"
  | Some(init) -> str ^ (Printf.sprintf ", initial value = %s " (sexpr2string (fst init)))
 
let rec line_return indent =
   if indent <1 then  "\n"
   else Printf.sprintf "%s   "  (line_return (indent-1))


(*pp a list of commands , remember a proc body is a block*)
let rec pp_block funb (printpos:bool) indent =  (*iter on block list = list of scommands*)
  List.fold_left (fun bef sc -> Printf.sprintf "%s %s" bef (pp_s_command_e sc printpos indent)) "" funb
  and
    pp_s_command_e sce (printpos:bool) indent =
    let stmt,ext=sce in
    let str = if printpos then Printf.sprintf("Smt @ pos=%s") (Localizing.extent_to_string ext) else "" in
    match stmt with
    | Sc_arrayassign(a,key,value) -> 
                               str^Printf.sprintf ("%s(assign)  %s[%s] := %s") (line_return indent) (sexpr2string (fst a)) (sexpr2string (fst key)) (sexpr2string (fst value))

    | Sc_assign(v,e) -> str^Printf.sprintf ("%s(assign)  %s := %s") (line_return indent) (sexpr2string (fst v)) (sexpr2string (fst e))
    | Sc_if(e,b1,b2) -> 
      if b2 != [] then 
        str^Printf.sprintf ("%s(testT)   if(%s) %s%s(testF)   else%s") (line_return indent) (sexpr2string (fst e)) (pp_block b1 printpos (indent+1)) (line_return indent) (pp_block b2 printpos (indent+1))
      else 
        str^Printf.sprintf ("%s(test)    if(%s) %s") (line_return indent) (sexpr2string (fst e)) (pp_block b1 printpos (indent+1))
    | Sc_while(e,b) -> str^Printf.sprintf  ("%s(while)   while(%s) %s") (line_return indent) (sexpr2string (fst e)) (pp_block b printpos (indent+1))
    | Sc_proc_call(call, arg)-> str^Printf.sprintf ("%s(call)    %s") (line_return indent) "calltodo"
    | Sc_assert(e) -> str^Printf.sprintf ("%s(assert)  %s") (line_return indent) (sexpr2string (fst e))
    | Sc_assume(e) -> str^Printf.sprintf ("%s(assume)  %s")  (line_return indent) (sexpr2string (fst e))
    | Sc_arrayinsert(tab, index, value) -> str^Printf.sprintf ("%s(insert)  %s.insert(%s, %s)")  (line_return indent) (sexpr2string (fst tab)) (sexpr2string (fst index)) (sexpr2string (fst value))
    | Sc_arrayremove(tab, index) -> str^Printf.sprintf ("%s(remove)  %s.remove(%s)")  (line_return indent) (sexpr2string (fst tab)) (sexpr2string (fst index))
    | Sc_arrayclear(tab) -> str^Printf.sprintf ("%s(clear)   %s.clear()")  (line_return indent) (sexpr2string (fst tab))
  

(*Print a class : the variables and functions*)
let pp_class aclass = 
  let first = Printf.sprintf "****Class %s****%sVariables :" aclass.s_class_name (line_return 1) in
  let deb = List.fold_left (fun bef var -> Printf.sprintf "%s%s%s %s" bef (line_return 2)  (type2string var.s_var_type) (var.s_var_name)) first aclass.s_class_variables in
  let funcs = List.fold_left (fun bef proc -> Printf.sprintf "%s%s%s %s\n" bef (line_return 2)  (proc.s_proc_name) (pp_block proc.s_proc_body false 3)) "" aclass.s_class_functions in
  deb ^ (Printf.sprintf "%sFunctions :\n" (line_return 1)) ^ funcs
		
(*Print a program : all classes*) 
let pp_s_program (prog : s_program) =
  let tmp = Printf.sprintf "--------------------- Pretty Print -----------------------\n" in
  List.fold_left (fun bef aclass -> Printf.sprintf "%s\n%s\n" bef (pp_class aclass)) tmp prog


