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
