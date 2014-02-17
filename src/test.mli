(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Test cases for the Countdown rounds. *)

(** {2 Type definitions} *)

(** Test cases for the Letters and Numbers rounds. *)
type t =
| Letters of string * string
(** Characters, solution. *)
| Numbers of int list * int * int * string option
(** Numbers, target, best answer, equation (optional). *)

(** {2 Testing routines} *)

(** Test whether a Letters or Numbers round produces the expected solution. *)
val run : letters:Countdown.Letters.c -> numbers:Countdown.Numbers.c
  -> log:bool -> t -> bool

(** {2 Test cases} *)

(** A suite of test cases for both the Letters and Numbers rounds. *)
val suite : t list

(** Run the suite of test cases. *)
val run_suite : dict:string -> log:bool -> unit

(** Run manual test cases that prompt the user for input. *)
val run_manual : dict:string -> unit
