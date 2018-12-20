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

val make_default_config : unit -> config_t
val  read_args : unit -> config_t
val string_config : config_t -> string
