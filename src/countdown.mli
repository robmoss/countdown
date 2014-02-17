(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Logic and solvers for playing the different Countdown rounds. *)

(** Dictionaries that support searching for longest words. *)
module Dictionary : sig

  (** {2 Type definitions} *)

  (** The dictionary type, which encompasses a list of words. *)
  type t

  (** {2 Initialisation} *)

  (** Load a dictionary from an input channel. *)
  val from_channel : in_channel -> t

  (** Load a dictionary from a file. *)
  val from_file : string -> t

  (** {2 Inspection} *)

  (** Print all words in a dictionary to an output channel. *)
  val print : t -> out_channel -> unit

  (** {2 Filtering} *)

  (** Only retain words that can be spelt using the given list of characters
      (multiple uses of a character are forbidden). *)
  val filter : t -> char list -> t

  (** {2 Searching} *)

  (** Return the longest word(s) in the dictionary. *)
  val longest : t -> string list

  (** Return the longest word(s) in the dictionary that are spelt using only
      the given list of characters (multiple uses are forbidden). *)
  val find_longest : t -> char list -> string list

  (** {2 String conversions} *)

  (** Convert a string to a list of characters. *)
  val explode : string -> char list

  (** Convert a list of characters to a string. *)
  val implode : char list -> string

end

(** Random selection of numbers and letters. *)
module AtRandom : sig

  (** Generate a population from a frequency list. *)
  val popn_of_freqs : ('a * int) list -> 'a list

  (** Randomly select an element from a population.
      Return both the selected value and the remaining population.
      The pseudo-random number generator ([Random]) should be initialised
      {b before} calling this function. *)
  val choice : 'a list -> 'a * 'a list

  (** Randomly select an integer within the given (inclusive) bounds.
      The pseudo-random number generator ([Random]) should be initialised
      {b before} calling this function. *)
  val int : min:int -> max:int -> int

end

(** Logic for the Letters round. *)
module Letters : sig

  (** {2 Type definitions} *)

  (** The parameters of the Letters round. *)
  type c = {
    dict: Dictionary.t;
    (** The list of valid words. *)
    vowels: char list;
    (** The list of available vowels. *)
    consonants: char list;
    (** The list of available consonants. *)
    per_round: int;
    (** How many letters must be selected for a single round. *)
    max_vowels: int;
    (** The maximum number of vowels that can be selected. *)
    max_consonants: int;
    (** The maximum number of consonants that can be selected. *)
  }

  (** The state of a Letters round. *)
  type t

  (** {2 Initialisation} *)

  (** The default options, which describe the standard Letters game. *)
  val config : dict_file:string -> c

  (** Start a new round of the Letters game. *)
  val init : c -> t

  (** {2 Game state} *)

  (** How many letters remain to be selected. *)
  val remaining : t -> int

  (** The letters that have been selected so far. *)
  val letters : t -> char list

  (** Whether the player can select any more vowels. *)
  val more_vowels : t -> bool

  (** Whether the player can select any more consonants. *)
  val more_consonants : t -> bool

  (** {2 Game actions} *)

  (** Add a specific letter to the list of selected letters. *)
  val add_letter : t -> char -> t

  (** Randomly select a vowel, using {!AtRandom.choice}. *)
  val add_random_vowel : t -> char * t

  (** Randomly select a consonant, using {!AtRandom.choice}. *)
  val add_random_consonant : t -> char * t

  (** {2 Solutions} *)

  (** Return a list of the longest words that can be spelt using the selected
      letters for the round. *)
  val solve : t -> string list

end

(** Logic for the Numbers round. *)
module Numbers : sig

  (** {2 Type definitions} *)

  (** The parameters of the Numbers round. *)
  type c = {
    large: int list;
    (** The list of available large numbers. *)
    small: int list;
    (** The list of available small numbers. *)
    per_round: int;
    (** How many numbers must be selected for a single round. *)
    max_large: int;
    (** The maximum number of large numbers that can be selected. *)
    max_small: int;
    (** The maximum number of small numbers that can be selected. *)
  }

  (** The state of a Numbers round. *)
  type t

  (** {2 Initialisation} *)

  (** The default options, which describe the standard Numbers game. *)
  val config : c

  (** Start a new round of the Numbers game, with a target chosen at random
      using {!AtRandom.choice}. *)
  val init : c -> t

  (** {2 Game state} *)

  (** How many numbers remain to be selected. *)
  val remaining : t -> int

  (** The numbers that have been selected so far. *)
  val numbers : t -> int list

  (** Whether the player can select any more large numbers. *)
  val more_large : t -> bool

  (** Whether the player can select any more small numbers. *)
  val more_small : t -> bool

  (** The target value. *)
  val target : t -> int

  (** {2 Game actions} *)

  (** Add a specific number to the list of selected numbers. *)
  val add_number : t -> int -> t

  (** Randomly select a large number, using {!AtRandom.choice}. *)
  val add_random_large : t -> int * t

  (** Randomly select a small number, using {!AtRandom.choice}. *)
  val add_random_small : t -> int * t

  (** Override the target value. *)
  val set_target : t -> int -> t

  (** {2 Solutions} *)

  (** Return an optimal solution [(value, formula)] for the round. *)
  val solve : t -> int * string

end
