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
                                         along with Vaphor.  If not, see <https://www.gnu.org/licenses/>.
                                                                            *)


open Simple_java_syntax
open Format
open Config
open Printexc
open Filename
open Horn_translate

(****** Parsing -> IR *****)
exception Lex_error

let parse_and_translate config =
  (*Checking Existence of input file*)
  if String.compare config.f_name "" = 0 then failwith ("No input file. Filename read is \""^config.f_name^"\"");
  Localizing.current_file_name := config.f_name;

  (*Opening file*)
  let f_desc = try open_in config.f_name 
  with 
    | Sys_error(e) -> failwith ("Impossible to open file. Filename read is \""^config.f_name^"\"") in

  (* Lexing *)
  let lexbuf = Lexing.from_channel f_desc in
  let java_prog = 
    (*Parsing*)
    try Java_parser.program Java_lexer.token lexbuf
    (*Retrieving where the error is*)
    with exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        let tail = Java_lexer.ruleTail "" lexbuf in
        failwith (Printf.sprintf "Problem when parsing (probably due to characters just before) %s %i %i" (tok^tail) cnum line )  (*raise (Error (exn,(line,cnum,tok)))*)
      end in

  (*Converting from java syntax to simple java syntax*)
  Simple_java_translate.tr_java_prog java_prog 
    


(****** MAIN *****)
				       
let _  =
  (*Show where exceptions comme from*)
  Printexc.record_backtrace(true);

  try
    (*Read arguments*)
    let cf = read_args() in

    (*Only show where expressions come from and show filename when debug is activated*)
    Printexc.record_backtrace(cf.debug);
    if cf.debug then Printf.printf "File is %s\n" cf.f_name;

    (*Retrieving program in simplified java syntax*)
    let simple_java_prog = parse_and_translate cf  in
  
    (*If debug, create log file and print program*)
    if cf.debug then begin 
      let fname = Filename.basename cf.f_name in
      let dname = Filename.dirname cf.f_name in
      let logFile = open_out (Printf.sprintf "%s/log%s.txt" dname (Filename.chop_extension fname)) in
      Printf.fprintf logFile "%s" (string_config cf);
      Printf.printf "%s" (string_config cf);
      Printf.printf "%s" (Simple_java_syntax.pp_s_program simple_java_prog);
      Printf.printf "Parsing and translating done\n";
    end;
    let abs_vars = get_abstract_vars simple_java_prog cf in
    let fs = get_formulas simple_java_prog abs_vars cf in
    let funcs = get_funs() in

    (*Getting stats*)
    let stat_str = Printf.sprintf ";Number of predicates (nodes) = %i\n;Number of variables = %i\n;Number of clauses = %i\n;" 
                                (List.length funcs) (List.length abs_vars) (List.fold_left (fun i f -> match f with
                                                                                                       | F_comment(s) -> i
                                                                                                       | _ -> i+1) 0 fs) in

    (*Creating string with all abstract information*)
    let str = (Printf.sprintf "%s\n(set-logic HORN)\n" stat_str) ^ 
            (List.fold_left (fun bef f -> bef ^ (Printf.sprintf "%s\n" (write_fun_decl f))) "" funcs)^
            (List.fold_left (fun bef f -> bef ^ (Printf.sprintf "%s\n" (print_formula f))) "" fs) ^ "(check-sat)\n" in

    (*Creating smt2 file and printing if debug*)
    if cf.debug then Printf.printf "%s" str; 
    let outfile = open_out cf.outputsmt_name in
    Printf.fprintf outfile "%s" str;
    ()
  with
    (*Whenever an exception is thrown, print expression and backtrace (empty if debug = false), and exit with -1 status*)
    | e -> Printf.printf "\n\nException : %s %s\n\n" (Printexc.to_string e) (Printexc.get_backtrace ()); exit (-1)



	       
