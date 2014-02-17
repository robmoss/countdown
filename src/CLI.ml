(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Command-line interface for playing the different Countdown rounds. *)

(** Logic for controlling a game from the command-line.*)
module Console = struct

  (** Repeatedly prompt the user (on stdin) with the given message, until they
      enter a valid choice. *)
  let rec prompt ~msg ~choices =
    let rec find_action line choices =
      match choices with
      | [] -> None
      | (str, f)::cs ->
        if str = line then
          Some f
        else
          find_action line cs
    in
    Printf.printf "%s" msg;
    let line = read_line () in
    match find_action line choices with
    | Some f -> f
    | None -> prompt ~msg ~choices

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
  module Make (G : GAME) = struct

    type config = G.config

    let play ~config ~timer =
      (* Recursively update the state until the game is finished. *)
      let rec update state =
        if G.continue state then (
          let msg, choices = G.prompt state in
          let update_fn = prompt ~msg ~choices in
          let new_state = update_fn state in
          Printf.printf "\n  %s\n" (G.string_of_state new_state);
          update new_state
        ) else (
          state
        )
      in

      (* Obtain the state at the end of the game. *)
      let state = update (G.init config) in
      let final_state, final_msg = G.finish state in

      (* Print the final message, if any. *)
      ( match final_msg with
      | None -> ()
      | Some s -> Printf.printf "\n%s" s
      );

      (* Wait until the solution should be displayed. *)
      ( match timer with
      | None ->
        Printf.printf "\n%s" (G.soln_prompt final_state);
        ignore (read_line ())
      | Some secs ->
        Printf.printf "\n%s\n" (G.timer_prompt final_state secs);
        flush stdout;
        Unix.sleep secs
      );

      (* Print the optimal solution(s). *)
      let soln = G.solve final_state in
      let soln_strs = G.strings_of_soln soln in
      List.iter (fun s -> Printf.printf "\n  %s\n" s) soln_strs;

  end

end

module LettersRound = struct

  module Letters = Countdown.Letters

  type config = Letters.c
  type state = Letters.t
  type soln = string list

  let init = Letters.init

  let continue state =
    Letters.remaining state > 0

  let prompt state =
    let v = "v", fun state -> snd (Letters.add_random_vowel state)
    and c = "c", fun state -> snd (Letters.add_random_consonant state) in
    match Letters.(more_vowels state, more_consonants state) with
      | true, true ->
        "\nPick a consonant or vowel [cv]: ", [c; v;]
      | true, false -> "\nPick a vowel [v]: ", [v;]
      | false, true -> "\nPick a consonant [c]: ", [c;]
      | _ -> failwith "ERROR: cannot choose a vowel or consonant."

  let finish state =
    state, None

  let soln_prompt state =
    "Press enter to view the longest word(s): "

  let timer_prompt state delay =
    Printf.sprintf "You have %d seconds ..." delay

  let solve = Letters.solve

  let string_of_state state =
    let b = Buffer.create 18 in
    List.iter (fun ch -> Buffer.add_char b ch; Buffer.add_char b ' ')
      (Letters.letters state);
    Buffer.contents b

  let strings_of_soln soln =
    soln

end

module NumbersRound = struct

  module Numbers = Countdown.Numbers

  type config = Numbers.c
  type state = Numbers.t
  type soln = int * string

  let init = Numbers.init

  let continue state =
    Numbers.remaining state > 0

  let prompt state =
    let l = "l", fun state -> snd (Numbers.add_random_large state)
    and s = "s", fun state -> snd (Numbers.add_random_small state) in
    match Numbers.(more_large state, more_small state) with
    | true, true -> "\nPick a large or small number [ls]: ", [l; s;]
    | true, false -> "\nPick a large number [l]: ", [l;]
    | false, true -> "\nPick a small number [s]: ", [s;]
    | _ -> failwith "ERROR: cannot choose a large or small number."

  let finish state =
    state, Some (Printf.sprintf "The target is %d" (Numbers.target state))

  let soln_prompt state =
    "Press enter to view the best solution: "

  let timer_prompt state delay =
    Printf.sprintf "You have %d seconds ..." delay

  let solve = Numbers.solve

  let string_of_state state =
    let b = Buffer.create 24 in
    List.iter (fun n -> Buffer.add_string b (string_of_int n);
      Buffer.add_char b ' ')
      (Numbers.numbers state);
    Buffer.contents b

  let strings_of_soln (v, eq_str) =
    [Printf.sprintf "%d = %s" v eq_str;]

end


(** Command-line interface for the Letters round. *)
module Letters = Console.Make (LettersRound)

(** Command-line interface for the Numbers round. *)
module Numbers = Console.Make (NumbersRound)
