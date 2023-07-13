open! Core

(** [Lambda_soup_utilities] exposes some simple commands that use the Lambda
    Soup library (https://github.com/aantron/lambdasoup) to parse HTML. *)

val command : Command.t
val get_title : String.t -> String.t
