(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Play Countdown rounds interactively. *)

(** {2 Type definitions} *)

(** Command-line options. *)
type options = {
  dict: string;
  (** The filename of the word list. *)
  timer: bool;
  (** Whether to use a timer for each round. *)
  delay: int;
  (** The number of seconds allocated for a single round. *)
  test: bool;
  (** Whether to run test cases.*)
  manual: bool;
  (** Whether to run test cases that prompt the user for input.*)
}

(** {2 Options} *)

(** The default command-line options. *)
val default_options : unit -> options

(** Determine the appropriate game timer given the command-line options. *)
val timer_of_opts : options -> int option

(** {2 Playing} *)

(** Play a single Letters round. *)
val play_letters : options -> Countdown.Letters.c -> unit

(** Play a single Numbers round. *)
val play_numbers : options -> Countdown.Numbers.c -> unit

(** Play any number of rounds of either game. *)
val play_game : options -> unit

(** {2 Entry-point} *)

(** Parse the command-line options and either run test cases or allow the user
    to play any number of Countdown rounds (via {!play_game}).
    This function also initialises the pseudo-random number generator
    ([Random.self_init ()]). *)
val main : unit -> unit
