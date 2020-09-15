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

(* This file defines an abstract evaluator for Simple_java_Syntax *)

open Simple_java_syntax
open Simple_java_translate
open Localizing
open Config

(*0 for ext last, 1 for ext first, 2 for no ext*)
let prednaming = ref 0 

exception Excpt_maps

(*abstract types*)
type abstract_type = 
  | Tr_int
  | Tr_bool
  | Tr_array of abstract_type
  

(*abstract variable*)
type abstract_var =
{ 
  trans_initial_var : s_var;
  trans_type : abstract_type;
}

(*Abstract commands*)
type abstract_command =
| Tr_start of abstract_var list
| Tr_end of abstract_var list
| Tr_command of s_command_e * (abstract_var list) (*the abstract var list is the list of variables used by the command*)



(*Abstract expression*)
type abstract_expr =
| Fe_variable of abstract_var
| Fe_bool of bool
| Fe_int of int64
| Fe_unary of s_unary_op * abstract_expr
| Fe_binary of s_binary_op * abstract_expr * abstract_expr
| Fe_read of abstract_type (*Replaces ?r in old version*)
| Fe_random
| Fe_string_var of string
| Fe_tuple of abstract_expr list
| Fe_select of abstract_expr * abstract_expr
| Fe_store of abstract_expr * abstract_expr * abstract_expr
| Fe_insert of abstract_expr * abstract_expr * abstract_expr
| Fe_remove of abstract_expr * abstract_expr
| Fe_size of abstract_expr
| Fe_emptyarray of abstract_expr

(*A smt2 formula*)
type formula = 
(*A predicate in a formula is a command where each variable is replaced by an expression. 
  For example, start i j => affect i (i+1) will be translated by
  F_implies(F_node(start, fun v -> v), F_node(affect, fun v -> if (v == j) then (i+1) else v))*)
| F_node of abstract_command*(abstract_var -> abstract_expr) 
| F_implies of formula*formula
| F_and of formula list
| F_bformula of abstract_expr
| F_comment of string

(*Usefull function, the question is why is it not camls default...*)
let string_equals s1 s2 = (String.compare s1 s2)==0

(*Retrieves a command name*)
let print_node (command, extent) =
  let prefix =
        match command with
        | Sc_assign _ -> "assign"
        | Sc_arrayassign _ -> "arrayassign"
        (*| Sc_random_assign _ -> "randomassign"*)
        | Sc_if _ -> "if"
        | Sc_while _ -> "while"
        | Sc_proc_call _ -> "call"
        | Sc_assert _ -> "assert"
	| Sc_assume _ -> "assume"
        | Sc_arrayinsert _ -> "arrayinsert"
        | Sc_arrayremove _ -> "arrayremove"
        | Sc_arrayclear _ -> "arrayclear"
        (*| Sc_mapassign _ -> "assign_array"*)
      in
  let res = match !prednaming with
            | 0 ->  Printf.sprintf "%s_%d_%d_%d"
                      prefix
                      extent.extent_beg.locus_file_line
                      extent.extent_beg.locus_file_column
                      extent.extent_unique_id
            | 1 -> Printf.sprintf "%s_%d_%d_%d"
                      prefix
                      extent.extent_unique_id
                      extent.extent_beg.locus_file_line
                      extent.extent_beg.locus_file_column
            | 2 -> Printf.sprintf "%s_%d_%d_%d"
                      prefix
                      extent.extent_unique_id
                      extent.extent_beg.locus_file_line
                      extent.extent_beg.locus_file_column
           in
  res

(*Print a variable name given the variable and which position index we are considering.
  Only for position variables*)
(*let print_pos_v v i = Printf.sprintf "%sd%ip%i" v.trans_initial_var.s_var_name v.distinct_num i*)

(*Prints expression*)
let rec print_expr expr =
  match expr with
  | Fe_variable(v) -> begin match v.trans_type with
                      | Tr_int -> v.trans_initial_var.s_var_name^ " "
                      | Tr_bool -> v.trans_initial_var.s_var_name^ " "
                      (*| Tr_value(_) -> Printf.sprintf "%sd%iv "  v.trans_initial_var.s_var_name v.distinct_num
                      | Tr_position(positions) -> (fst (List.fold_left (fun (b,i) p -> 
                                             ((Printf.sprintf "%s%s " b  (print_pos_v v i)), i+1)) ("", 0) positions))*)
                      | Tr_array(t) -> v.trans_initial_var.s_var_name^ " "  end
  | Fe_string_var(s) -> s
  | Fe_bool(b) -> Printf.sprintf "%B" b
  | Fe_int(i) -> Printf.sprintf "%Ld" i
  | Fe_unary(Su_neg, exp) -> "(- "^(print_expr exp)^")"
  | Fe_unary(Su_not, exp) -> "(not "^(print_expr exp)^")"

  | Fe_binary(op, Fe_variable(v), e) -> begin match v.trans_type with
                                           (*| Tr_position(l) -> print_expr (Fe_binary(op, Fe_tuple(List.mapi (fun i p -> Fe_string_var(print_pos_v v i)) l), e))*)
                                           | _ -> print_expr (Fe_binary(op, Fe_string_var(print_expr (Fe_variable(v))), e)) end
  | Fe_binary(op, e, Fe_variable(v)) -> begin match v.trans_type with
                                           (*| Tr_position(l) -> print_expr (Fe_binary(op, e, Fe_tuple(List.mapi (fun i p -> Fe_string_var(print_pos_v v i)) l)))*)
                                           | _ -> print_expr (Fe_binary(op, e, Fe_string_var(print_expr (Fe_variable(v))))) end
  | Fe_binary(Sb_add, exp1, exp2) -> "(+ "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_sub, exp1, exp2) -> "(- "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_mul, exp1, exp2) -> "(* "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_div, exp1, exp2) -> "(/ "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_mod, exp1, exp2) -> "(mod "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_or, exp1, exp2) -> "(or "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_and, exp1, exp2) -> "(and "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_xor, exp1, exp2) -> "(xor "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_imp, exp1, exp2) -> "(=> "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_lt, Fe_tuple(l1), Fe_tuple(l2)) -> begin match (l1,l2) with
                                                    | [], [] -> ""
                                                    | [a], [b] -> print_expr (Fe_binary(Sb_lt, a, b))
                                                    | a::q, b::t -> print_expr (Fe_binary(Sb_or, Fe_binary(Sb_lt, a, b), 
                                                                                                 Fe_binary(Sb_and, Fe_binary(Sb_eq, a, b), 
                                                                                                                   Fe_binary(Sb_lt, Fe_tuple(q), Fe_tuple(t))))) 
                                                    | _ -> failwith "Tuples do not have same size" end
  | Fe_binary(Sb_lt, exp1, exp2) -> "(< "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_le, exp1, exp2) -> "(<= "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_gt, exp1, exp2) -> "(> "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_ge, exp1, exp2) -> "(>= "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Sb_eq, Fe_tuple(l1), Fe_tuple(l2)) -> begin match (l1,l2) with
                                                    | [], [] -> ""
                                                    | [a], [b] -> print_expr (Fe_binary(Sb_eq, a, b))
                                                    | a::q, b::t -> print_expr (Fe_binary(Sb_and, Fe_binary(Sb_eq, a, b),
                                                                                                  Fe_binary(Sb_eq, Fe_tuple(q), Fe_tuple(t)))) 
                                                    | _ -> failwith "Tuples do not have same size" end
  | Fe_binary(Sb_eq, exp1, exp2) -> "(= "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_read(t) -> "?r"
  | Fe_random -> "rnd"
  | Fe_tuple(l) ->  List.fold_left (fun bef e -> bef^" "^(print_expr e)) "" l	
  
  | Fe_select(tab, index) -> Printf.sprintf "(select %s %s)" (print_expr tab) (print_expr index)
  | Fe_store(tab, index, value) -> Printf.sprintf "(store %s %s %s)" (print_expr tab) (print_expr index) (print_expr value)
  | Fe_insert(tab, index, value) -> Printf.sprintf "(insert %s %s %s)" (print_expr tab) (print_expr index) (print_expr value)
  | Fe_remove(tab, index) -> Printf.sprintf "(remove %s %s)" (print_expr tab) (print_expr index)
  | Fe_size(tab) -> Printf.sprintf "(size %s)" (print_expr tab)
  | Fe_emptyarray(tab) -> Printf.sprintf "(clear %s)" (print_expr tab)


(*Prints command, usefull for comments*)
let print_command command =
    match fst command with
    | Sc_assign(v,e) -> Printf.sprintf ("(assign)  %s := %s") (sexpr2string (fst v)) (sexpr2string (fst e))
    | Sc_arrayassign(tab, index, value) -> Printf.sprintf ("(aassign) %s[%s] := %s") (sexpr2string (fst tab)) (sexpr2string (fst index)) (sexpr2string (fst value))
    (*| Sc_random_assign(var, low, high) -> Printf.sprintf ("(rassign) %s := Support.random(%s,%s)") (sexpr2string (fst var)) (sexpr2string (fst low)) (sexpr2string (fst high))*)
    | Sc_if(e,b1,b2) -> 
        Printf.sprintf "(if)      if(%s)" (sexpr2string (fst e))
    | Sc_while(e,b) -> Printf.sprintf  ("(while)   while(%s)") (sexpr2string (fst e))
    | Sc_proc_call(call, arg)-> Printf.sprintf ("(call)    %s") "calltodo"
    | Sc_assert(e) -> Printf.sprintf ("(assert)  %s") (sexpr2string (fst e))
    | Sc_assume(e) -> Printf.sprintf ("(assume)  %s")  (sexpr2string (fst e))
    | Sc_arrayinsert(tab, index, value) -> Printf.sprintf ("(insert)  insert(%s, %s, %s)") (sexpr2string (fst tab)) (sexpr2string (fst index)) (sexpr2string (fst value))
    | Sc_arrayremove(tab, index) -> Printf.sprintf ("(remove)  remove(%s, %s)") (sexpr2string (fst tab)) (sexpr2string (fst index))
    | Sc_arrayclear(tab) -> Printf.sprintf ("(clear)   clear(%s)") (sexpr2string (fst tab))


(*Prints type for z3*)
let rec print_type t =
  match t with
  | Tr_int -> "Int"
  | Tr_bool -> "Bool"
(*  | Tr_value(vt) -> print_type vt*)
  | Tr_array(v) -> Printf.sprintf "(Array Int %s)" (print_type v)
  | _ -> failwith "Printing a non native type"

(*Does a union of the elements of both list. Allows lists to be used as Sets*)
let list_union l1 l2 =
  List.fold_left (fun l (str,t) -> if List.exists (fun (s, ty) -> string_equals s str) l then l else (str,t)::l) l1 l2

(*Get variables used in an expression and their types*)
let rec get_expr_variables e t =
  match e with
  | Fe_variable(v) -> begin match v.trans_type with
                      | Tr_int -> [(v.trans_initial_var.s_var_name, Tr_int)]
                      | Tr_bool -> [(v.trans_initial_var.s_var_name, Tr_bool)]
                      (*| Tr_value(ty) -> [(Printf.sprintf "%sd%iv"  v.trans_initial_var.s_var_name v.distinct_num, ty)]
                      | Tr_position(positions) -> List.mapi (fun i p -> (print_pos_v v i, p)) positions*)
                      | Tr_array(t)-> [(v.trans_initial_var.s_var_name, Tr_array(t))] end
  | Fe_string_var(s) -> [(s, t)]
  | Fe_bool(b) -> []
  | Fe_int(i) -> []
  | Fe_unary(_, exp) -> get_expr_variables exp t
  | Fe_binary(Sb_add, exp1, exp2) 
  | Fe_binary(Sb_sub, exp1, exp2) 
  | Fe_binary(Sb_mul, exp1, exp2) 
  | Fe_binary(Sb_div, exp1, exp2) 
  | Fe_binary(Sb_mod, exp1, exp2) 
  | Fe_binary(Sb_lt, exp1, exp2) 
  | Fe_binary(Sb_le, exp1, exp2) 
  | Fe_binary(Sb_gt, exp1, exp2) 
  | Fe_binary(Sb_ge, exp1, exp2) 
  | Fe_binary(Sb_eq, exp1, exp2) -> list_union (get_expr_variables exp1 Tr_int) (get_expr_variables exp2 Tr_int)
  | Fe_binary(Sb_and, exp1, exp2) 
  | Fe_binary(Sb_or, exp1, exp2) 
  | Fe_binary(Sb_xor, exp1, exp2) -> list_union (get_expr_variables exp1 Tr_int) (get_expr_variables exp2 Tr_bool)
  | Fe_binary(Sb_imp, exp1, exp2) -> list_union (get_expr_variables exp1 Tr_int) (get_expr_variables exp2 Tr_bool)
  | Fe_read(ty) -> [("?r", ty)]
  | Fe_random -> [("rnd", t)]
  | Fe_tuple(l) ->  begin match t with
                    (*| Tr_position(tl) -> List.fold_left2 (fun s exp te -> list_union s (get_expr_variables exp te)) [] l tl*)
                    | _ -> List.fold_left (fun s exp -> list_union s (get_expr_variables exp t)) [] l end
  | Fe_select(tab, l) -> (get_expr_variables tab (Tr_array(Tr_int)) ) @ (get_expr_variables l Tr_int)
  | Fe_store(tab, l, e) -> (get_expr_variables tab (Tr_array(Tr_int))) @ (get_expr_variables l Tr_int)@(get_expr_variables e Tr_int)
  | Fe_insert(tab, index, value) -> (get_expr_variables tab (Tr_array(Tr_int))) @ (get_expr_variables index Tr_int)@(get_expr_variables value Tr_int)
  | Fe_remove(tab, index) -> (get_expr_variables tab (Tr_array(Tr_int))) @ (get_expr_variables index Tr_int)
  | Fe_size(tab) -> (get_expr_variables tab (Tr_array(Tr_int)))
  | Fe_emptyarray(tab) -> (get_expr_variables tab (Tr_array(Tr_int)))

(*Get variable used in a formula*)
let rec get_used_variables f = 
  match f with
  | F_node(Tr_command(command, vars), func) -> List.fold_left (fun set v -> list_union set (get_expr_variables (func v) (v.trans_type))) [] vars
  | F_node(Tr_start(vars), func) ->  List.fold_left (fun set v -> list_union set (get_expr_variables (func v) (v.trans_type))) [] vars
  | F_node(Tr_end(vars), func) -> List.fold_left (fun set v -> list_union set (get_expr_variables (func v) (v.trans_type))) [] vars
  | F_implies(f1, f2) -> list_union (get_used_variables f1) (get_used_variables f2) 
  | F_and(l) -> List.fold_left (fun bef f -> list_union bef (get_used_variables f)) [] l
  | F_bformula(e) -> (get_expr_variables e Tr_bool)
  | F_comment(s) -> failwith "comments not possible here"

(*Prints a formula*)
let rec print_formula formula=
  let rec print_subformula f = (match f with
    | F_node(Tr_command(command, vars), func) -> "("^(print_node command)^(List.fold_left (fun bef var -> bef^" "^(print_expr (func var))) "" vars)^")"
    | F_node(Tr_start(vars), func) -> "(start"^(List.fold_left (fun bef var -> bef^" "^(print_expr (func var))) "" vars)^")"
    | F_node(Tr_end(vars), func) -> "(end"^(List.fold_left (fun bef var -> bef^" "^(print_expr (func var))) "" vars)^")"
    | F_implies(f1, f2) -> "( => "^(print_subformula f1)^" "^(print_subformula f2)^")"
    | F_and(l) -> if List.length l == 0 then "true" else "( and"^(List.fold_left (fun bef f -> bef^" "^(print_subformula f)) "" l)^")"
    | F_bformula(e) -> (print_expr e)
    | F_comment(s) -> failwith "comments not possible here") in
  match formula with
  | F_comment(s) -> ";"^s
  | _ -> "(assert (forall ("^ 
         (List.fold_left (fun bef (v_name, t) -> Printf.sprintf "%s(%s %s) " bef v_name (print_type t)) "" (get_used_variables formula)) 
         ^ ") " ^ (print_subformula formula) ^ "))"


(*Retrieve the bastract type of a variable*)
let rec get_abstract_type = function
  |  St_int -> Tr_int
  |  St_bool -> Tr_bool
  |  St_void -> failwith "Variable with type void not allowed"
  |  St_array(value) -> Tr_array(get_abstract_type value)
  (*|  St_tuple(l) -> failwith "Variable of type tuple not allowed"*)
  (*|  St_map(key, value) -> failwith "Variable of type map can not be abstracted"*)

(*Creates the list (func 0)::(func 1)::func(2)::...::func(size-1) *)
let rec create_list size func = 
  match size with
  | 0 -> []
  | k -> (create_list (size-1) func)@[(func (k-1))]

(*Returns the abstract variable linked to a normal variable, considering the dstinct number and whether you want the value or the position variable
  Can be used on all kind of variables, if the variable is not a map, the parameters distinct and is_pos are ignored*)
let get_abstract_variable s_var distinct is_pos =
  match s_var.s_var_type with
  (*| St_map(St_tuple(key), value) -> if is_pos then { trans_initial_var = s_var; 
                                                     trans_type = Tr_position(List.map get_abstract_type key);
                                                     distinct_num = distinct;
                                                   }
                                    else 
                                        { trans_initial_var = s_var; 
                                          trans_type = Tr_value(get_abstract_type value);
                                          distinct_num = distinct;
                                        }*)
  | t -> {trans_initial_var = s_var; trans_type = get_abstract_type t;}

(*Retrieve all abstract var from a program*)
let get_abstract_vars prog cf =
  if List.length prog != 1 then failwith "Abstraction can only deal with one class at the moment";
  let vars = (List.hd prog).s_class_variables in
  if cf.distinct_i >=0 then 
    (List.flatten (List.map (fun var -> match var.s_var_type with
                                 (*| St_map(St_tuple(key), value) -> List.flatten (create_list cf.distinct_i (fun i -> (get_abstract_variable var i true)::[(get_abstract_variable var i false)]))*)
                                 | t -> [get_abstract_variable var (-1) true]) vars))
  else 
    (List.flatten (List.map (fun var -> match var.s_var_type with
                                 (*| St_map(St_tuple([St_int]), value) -> [{ trans_initial_var = var; 
                                                                          trans_type = Tr_intarray;
                                                                          distinct_num = -1;
                                                                        }]
                                 | St_map(_, value) -> failwith "Only intarrays allowed when distinct <0"*)
                                 | t -> [get_abstract_variable var (-1) true]) vars))


(*Converts an expression*)
let rec tr_expr (expr,ext) = 
  match expr with
  | Se_const(Sc_int(i)) -> Fe_int(i)
  | Se_const(Sc_bool(b)) -> Fe_bool(b)
  | Se_var(var) -> Fe_variable(get_abstract_variable var (-1) true)
  | Se_unary(op, e) -> Fe_unary(op, tr_expr e)
  | Se_binary(op, e1, e2) -> Fe_binary(op, tr_expr e1, tr_expr e2)
  | Se_arrayaccess(tab, index) -> Fe_select(tr_expr tab, tr_expr index)
  | Se_arraysize(tab) -> Fe_size(tr_expr tab)
  | Se_random_bounded(low, high) -> failwith "should not encounter random here"
  | Se_random -> failwith "should not encounter random here"
  | H_store(tab, index, value) -> Fe_store(tr_expr tab, tr_expr index, tr_expr value)

(*Allows better error messaging by knowing if an expression has maps*)
let has_maps expr = try
                      let _ = tr_expr expr in
                      false
                    with
                    | Excpt_maps -> true
                    | e -> raise e

(*Helper function for the creation of d0p < d1p < expr < d2p...
  order p expr tab distinct returns the following :
  (tabd0::tabd1::...tabd(p-1)::...exp::tabdp...tabd(distinct-1))
  where each element of the list is a function that to true returns the position variable and to false returns the value variable.
  The value variable of exp is Fe_read and the position is expr*)
let order p expr tab distinct= 
  let l = create_list distinct (fun i -> (fun bp -> Fe_variable (get_abstract_variable tab i bp))) in
  let (l1,l2) = List.partition (fun f -> match f true with
                                         (*| Fe_variable(v) -> v.distinct_num >=p*)
                                         | _ -> failwith "Not a variable") l in
  let typ = (get_abstract_variable tab 0 false).trans_type in
  l2 @ [fun bp -> if bp then tr_expr expr else Fe_read(typ)] @ l1

(*Creates the list of all adjacent pairs*)
let rec list_of_adj_pairs l =
  match l with
  | [] -> []
  | [a] -> []
  | a::b::q -> (a,b)::(list_of_adj_pairs (b::q))

(*returns the formulas for d0p < d1p < expr < d(distinct-1)p... where expr is in position p*)
let expr_position p expr tab distinct = 
  let odr = List.map (fun f -> f true) (order p expr tab distinct) in
  List.map (fun (x,y) -> F_bformula(Fe_binary(Sb_lt, x, y))) (list_of_adj_pairs odr)

(*Returns whether the variable is a position variable or a value variable.Should only be used for map variables*)
let is_pos v = match v.trans_type with
  (*| Tr_value(_) -> false
  | Tr_position(_) -> true*)
  | _ -> failwith "is pos should not be called with non map types"

(* The idea is to have a node with 1 more variable which is the expr variable in position p (so the total number of variables is distinct +1). 
   Then, we generate all distinct +1 possibilities of removing one of the variables.
   For example :
   command_pos start 2 expr tab 3 ->
     //Image : d0p d0v d1p d1v expr Fe_read d2p d2v
     //Return value
     [ start d1p d1v expr Fe_read d2p d2v
       start d0p d0v expr Fe_read d2p d2v
       start d0p d0v d1p d1v d2p d2v
       start d0p d0v d1p d1v expr Fe_read ]
   Remark : As we do not have the positions of the variables, the rewritting rule consists in changing the distinct number*)
let command_pos c p expr tab distinct =
  let odr = order p expr tab distinct in
  create_list (distinct+1) (fun i -> F_node(c, fun v -> 
                                                    (*if (string_equals v.trans_initial_var.s_var_name tab.s_var_name) then begin
                                                      if v.distinct_num < i then (List.nth odr v.distinct_num) (is_pos v)
                                                      else (List.nth odr (v.distinct_num+1)) (is_pos v) end
                                                    else*) Fe_variable(v)))

(*Same as expr_position except that e now replaces the distinct p*)
let order_matched p e tab distinct =
  let l = create_list distinct (fun i -> if i != p then Fe_variable(get_abstract_variable tab i true) else tr_expr e) in
  let pairs = list_of_adj_pairs l in
  List.map (fun (x,y) -> F_bformula(Fe_binary(Sb_lt, x, y))) pairs   

(*Helper functions for node declaration (avoids having to same nodes) and allows retrieval of all nodes*)
let (declare_fun, get_funs) =    
  let helper () = 
    let l = ref [] in
    let decl command =
      let (str, vars) = match command with
        | Tr_start(vars) -> ("start", vars)
        | Tr_end(vars) -> ("end", vars)
        | Tr_command(command, vars) -> (print_node command, vars)
      in
      if List.exists (fun (c, v) -> (string_equals c str)) (!l) then failwith (Printf.sprintf "Command already declared : %s" str);
      let types = List.flatten (List.map (fun v -> match v.trans_type with
                                                   | Tr_int -> [Tr_int]
                                                   | Tr_bool -> [Tr_bool]
                                                   (*| Tr_value(t) -> [t]
                                                                        | Tr_position(l) -> l*)
                                                   | Tr_array(t) -> [Tr_array(t)]) vars) in
      
      l:=(str, types)::(!l)   in
    let get ()= !l in      
    (decl, get)  in
  helper()    

(*Write fun declaration in smt2 format*)
let write_fun_decl (f_name, types) = 
  Printf.sprintf "(declare-fun %s (%s) Bool)" f_name (List.fold_left (fun bef t -> bef ^ (print_type t)^" ") "" types)             


(*Create formula from block. Simply said, it iterates on the block and converts according to the article. next_node is the next node after this block.*)
let rec create_formulas block abstract_variables next_node distinct =
  let tr_b = List.map (fun command -> Tr_command(command, abstract_variables)) block in
  let pairs = list_of_adj_pairs (tr_b @ [next_node]) in
  let transformed = List.map (fun (c, next_command) -> match c with
     | Tr_command(command, _) -> declare_fun c; F_comment("\n;"^print_command command)::(match (fst command) with
       | Sc_assign(var, expr) -> 
         begin match tr_expr var with
         | Fe_variable(mv) -> 
             begin match fst expr with
             | Se_random -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> if v = mv then Fe_random else Fe_variable(v)))]
             | Se_random_bounded(low, high) -> [F_implies(F_and([F_node(c, fun v -> Fe_variable(v));
                                                                 F_bformula(Fe_binary(Sb_ge, Fe_random, tr_expr low));F_bformula(Fe_binary(Sb_lt, Fe_random, tr_expr high))]), 
                                                          F_node(next_command, fun v -> if v = mv then Fe_random else Fe_variable(v)))]
             | _ -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> if v = mv then tr_expr expr else Fe_variable(v)))]
             end
         | _ -> failwith "error"
         end
       (*| Sc_random_assign(var, low, high) -> failwith "error"*)
       | Sc_if(expr, [], []) -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> Fe_variable(v)))]

       | Sc_if(expr, a::q, []) -> if has_maps expr then failwith "Cannot dal with maps yet";
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(tr_expr expr)], 
                                          F_node(Tr_command(a, abstract_variables), fun v -> Fe_variable(v))) ::
                                 F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(Fe_unary(Su_not, tr_expr expr))], 
                                          F_node(next_command, fun v -> Fe_variable(v))) :: (create_formulas (a::q) abstract_variables next_command distinct)
       | Sc_if(expr, [], a::q) -> if has_maps expr then failwith "Cannot dal with maps yet";
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(tr_expr expr)], 
                                          F_node(next_command, fun v -> Fe_variable(v))) ::
                                 F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(Fe_unary(Su_not, tr_expr expr))], 
                                          F_node(Tr_command(a, abstract_variables), fun v -> Fe_variable(v))) :: (create_formulas (a::q) abstract_variables next_command distinct)

       | Sc_if(expr, a::q, b::t) -> if has_maps expr then failwith "Cannot dal with maps yet";
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(tr_expr expr)], 
                                          F_node(Tr_command(a, abstract_variables), fun v -> Fe_variable(v)))::
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(Fe_unary(Su_not, tr_expr expr))], 
                                          F_node(Tr_command(b, abstract_variables), fun v -> Fe_variable(v))) :: ((create_formulas (a::q) abstract_variables next_command distinct) 
                                                                                                               @ (create_formulas (b::t) abstract_variables next_command distinct))

                                
       | Sc_while(expr, a::q) ->  if has_maps expr then failwith "Cannot deal with maps yet";
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(tr_expr expr)], 
                                          F_node(Tr_command(a, abstract_variables), fun v -> Fe_variable(v)))::
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(Fe_unary(Su_not,tr_expr expr))], 
                                          F_node(next_command, fun v -> Fe_variable(v))) :: (create_formulas (a::q) abstract_variables c distinct)
       | Sc_while(expr, []) ->  if has_maps expr then failwith "Cannot deal with maps yet";
                                F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(tr_expr expr)], 
                                          F_node(c, fun v -> Fe_variable(v)))::
                                [F_implies(F_and [F_node(c, fun v -> Fe_variable(v)); F_bformula(Fe_unary(Su_not, tr_expr expr))], 
                                          F_node(next_command, fun v -> Fe_variable(v)))]
       | Sc_proc_call(f, arg) -> failwith "Not Implemented yet"
       | Sc_assert(expr) -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_bformula(tr_expr expr)); F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> Fe_variable(v)))]
       | Sc_assume(expr) -> [F_implies(F_and([F_node(c, fun v -> Fe_variable(v)); F_bformula(tr_expr expr)]), F_node(next_command, fun v -> Fe_variable(v)))]
       | Sc_arrayassign(tab, index, value) -> 
         begin match tr_expr tab with
         | Fe_variable(t) -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> if v = t then Fe_store(Fe_variable(t), tr_expr index, tr_expr value) else Fe_variable(v)))]
         | Fe_select(Fe_variable(t), ind) -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, 
                                                          fun v -> if v = t then  Fe_store(Fe_variable(t), ind, Fe_store(Fe_select(Fe_variable(t), ind), tr_expr index, tr_expr value))
                                                                            else Fe_variable(v)))]
         | _ -> failwith "error"
         end
       | Sc_arrayinsert(tab, index, value) -> 
         begin match tr_expr tab with
         | Fe_variable(t) -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> if v = t then Fe_insert(Fe_variable(t), tr_expr index, tr_expr value) else Fe_variable(v)))]
         | Fe_select(Fe_variable(t), index) -> failwith "todo"
         | _ -> failwith "error"
         end
       | Sc_arrayremove(tab, value) -> failwith "error"
       | Sc_arrayclear(tab) -> 
         begin match tr_expr tab with
         | Fe_variable(t) -> [F_implies(F_node(c, fun v -> Fe_variable(v)), F_node(next_command, fun v -> if v = t then Fe_emptyarray(Fe_variable(t)) else Fe_variable(v)))]
         | Fe_select(Fe_variable(t), index) -> failwith "todo"
         | _ -> failwith "error"
         end
       )
     | _ -> failwith "Tr_start or Tr_end cannot be created from block") pairs in
  List.flatten transformed


(*Creates the initial tab order : d0p< d1p < d2p ...*)
let initial_tabs_order abstract_variables distinct=
  List.flatten (List.map (fun v -> match v.trans_type with
                     (*| Tr_position(l) -> if v.distinct_num >0 then [F_bformula(Fe_binary(Sb_lt, Fe_variable(get_abstract_variable v.trans_initial_var (v.distinct_num-1) true), Fe_variable(v)))] else []*)
                     | _ -> []) abstract_variables)


(*Returns the formulas*)
let get_formulas prog abstract_variables cf =
  if List.length prog != 1 then failwith "Abstraction can only deal with one class at the moment";
  let initialize = List.find (fun func -> (String.compare func.s_proc_name "_initialize" ==0)) (List.hd prog).s_class_functions in
  let main = List.find (fun func -> (String.compare func.s_proc_name "main" ==0)) (List.hd prog).s_class_functions in
  let start_c = Tr_start(abstract_variables) in
  let end_c = Tr_end(abstract_variables) in
  declare_fun (start_c);
  declare_fun (end_c);
  let allproc = if cf.ignore_init then main.s_proc_body else initialize.s_proc_body @ main.s_proc_body in
  let formula = (F_implies(F_and(initial_tabs_order abstract_variables cf.distinct_i), F_node(start_c, (fun v -> Fe_variable(v)))))::
                (F_implies(F_node(start_c, fun v -> Fe_variable(v)), F_node(Tr_command(List.hd allproc, abstract_variables), fun v -> Fe_variable(v))))::
                (create_formulas allproc abstract_variables (end_c) cf.distinct_i) in
  formula


