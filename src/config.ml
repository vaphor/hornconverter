open Format

(****** Command line + Software configuration *****)
type config_t = {
  f_name:string;
  outputsmt_name:string;
  outputdot_name:string;
  distinct_i:int;
  eval:bool; (*symbolic eval*)
  debug:bool;
  version:string;
  ignore_init:bool;
}

exception Version
exception Usage of string
exception NotFound of string
			

let set_outputdot config (s:string) = config := {!config with outputdot_name=s}
let set_outputsmt config (s:string) = config := {!config with outputsmt_name=s}
let set_debug config () = config := {!config with debug=true}
let set_eval config () = config := {!config with eval=true}
let set_ignore_init config () = config := {!config with ignore_init=true}

let set_fname config (s:string) =  
  if Sys.file_exists s
  then config := {!config with f_name=s}
  else raise (NotFound s) 

let set_di config nb =
  if (nb < -1)
  then raise (Usage "nb distinguished els should be >= -1 (-1 = normal arrays)")
  else config := {!config with distinct_i=nb}
					   
let make_default_config () = {
  f_name="";
  outputsmt_name="foo.smt2";
  outputdot_name="foo.dot";
  distinct_i = 1;
  debug=false;
  eval=false;
  ignore_init = false;
  version="1.0 - Sept 2016";
}
			       
let string_config cf =
  Printf.sprintf "inputfile=%s,di=%d\n" cf.f_name cf.distinct_i
			       
let read_args () =
  let cf = ref (make_default_config()) in
  let speclist = 
    [
      ("--version",Arg.Unit (fun () -> fprintf std_formatter "java2horn Version %s@." !cf.version ; raise(Version)),": print version and exit");
      ("-debug", Arg.Unit (set_debug cf) ,": all debug info");
      ("-eval", Arg.Unit (set_eval cf) ,": evaluator (WIP)");
      ("-distinct", Arg.Int (set_di cf) ,": #distinguished elements in abstraction");
      ("-o", Arg.String (set_outputsmt cf) ,": outputfile, default is foo.smt2");
      ("--ignore-init", Arg.Unit (set_ignore_init cf) ,": ignores variable initializations in declarations");
    ] in
  let usage_msg = "Usage : ./java2horn [options] file" in
  try (Arg.parse speclist (set_fname cf) usage_msg; 
       if !cf.f_name = "" then begin Arg.usage speclist usage_msg ; raise (Usage usage_msg) end; 
       !cf 
      )
  with
  | Version -> exit(1)
  | Usage(_) -> exit(1)
 
