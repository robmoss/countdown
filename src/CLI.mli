(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Command-line interface for playing the different Countdown rounds. *)

(** Logic for controlling a game from the command-line.*)
module Console : sig

  (** Repeatedly prompt the user (on stdin) with the given message, until they
      enter a valid choice. *)
  val prompt : msg:string -> choices:(string * 'a) list -> 'a

  (** A game that can be played from the command-line. *)
  module type GAME = sig

    (** {3 Type definitions} *)

    (** Game-specific settings. *)
    type config

    (** State of the current round. *)
    type state

    (** Solution to the current round. *)
    type soln

    (** {3 Game state} *)

    (** Start a new round. *)
    val init : config -> state

    (** Indicate whether or not the round has ended. *)
    val continue : state -> bool

    (** {3 Actions} *)

    (** End the round, returning an optional message. *)
    val finish : state -> state * string option

    (** The solution to the current round. *)
    val solve : state -> soln

    (** {3 Prompts} *)

    (** Return a prompt and a list of valid responses, each of which is
        associated with an action that updates the game state. *)
    val prompt : state -> string * (string * (state -> state)) list

    (** The message to display before showing the solution. *)
    val soln_prompt : state -> string

    (** The message to display when playing a timed round. *)
    val timer_prompt : state -> int -> string

    (** {3 Output} *)

    (** A string representation of the current round. *)
    val string_of_state : state -> string

    (** A string representation of the solution. *)
    val strings_of_soln: soln -> string list
  end

  (** The command-line interface for a particular game. *)
  module type T = sig
    (** Game-specific settings. *)
    type config

    (** Play the game with the given settings and an optional timer for each
        round (measured in seconds). *)
    val play : config:config -> timer:int option -> unit
  end

  (** Create command-line interfaces for any game [G]. *)
  module Make (G : GAME) : T with type config = G.config

end

(** Command-line interface for the Letters round. *)
module Letters: Console.T with type config = Countdown.Letters.c

(** Command-line interface for the Numbers round. *)
module Numbers: Console.T with type config = Countdown.Numbers.c
