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

let default_options () =
  (* Set the default dictionary to "./share/data/twl06.txt", relative to the
     root of the repository. *)
  let base_dir = Filename.(dirname (dirname Sys.executable_name)) in
  let default_dict =
    List.fold_left Filename.concat base_dir ["share"; "data"; "twl06.txt";]
  in
  { dict = default_dict;
    timer = false;
    delay = 30;
    test = false;
    manual = false;
  }

let timer_of_opts opts =
  if opts.timer && opts.delay > 0 then
    Some opts.delay
  else
    None

(** {2 Playing} *)

(** Play a single Letters round. *)
let play_letters opts config =
  CLI.Letters.play ~config ~timer:(timer_of_opts opts)

(** Play a single Numbers round. *)
let play_numbers opts config =
  CLI.Numbers.play ~config ~timer:(timer_of_opts opts)

(** Play any number of rounds of either game. *)
let play_game opts =
  let config_l = Countdown.Letters.config ~dict_file:opts.dict
  and config_n = Countdown.Numbers.config in

  let timer =
    if opts.timer && opts.delay > 0 then
      Some opts.delay
    else
      None
  in

  let continue = ref true in
  let rec play_round () =
    let msg = "\nPlay a round of letters, numbers, or quit [lnq]: "
    and choices = [
      "l", (fun _ -> CLI.Letters.play ~config:config_l ~timer);
      "n", (fun _ -> CLI.Numbers.play ~config:config_n ~timer);
      "q", (fun _ -> continue := false); ]
    in
    CLI.Console.prompt ~msg ~choices ();
    if !continue then play_round ()
  in

  play_round ()

(** {2 Entry-point} *)

(** Parse the command-line options and either run test cases or allow the user
    to play any number of Countdown rounds (via {!play_game}).
    This function also initialises the pseudo-random number generator
    ([Random.self_init ()]). *)
let main () =
  let opts = ref (default_options ()) in
  let argspec = [
    "--dict", Arg.String (fun s -> opts := { !opts with dict = s; }),
      " Location of the word list";
    "--timer", Arg.Unit (fun () -> opts := { !opts with timer = true; }),
      " Apply a time limit to each round";
    "--delay", Arg.Int (fun i -> opts := { !opts with delay = i; }),
      " Set the round duration (seconds)";
    "--test", Arg.Unit (fun () -> opts := { !opts with test = true; }),
      " Run test cases";
    "--manual", Arg.Unit (fun () -> opts := { !opts with manual = true; }),
      " Run manual test cases (requires --test)\n";
  ] in
  Arg.parse (Arg.align argspec) (fun _ -> ())
    "\nUSAGE: play [--dict FILE --timer --delay SECS --test --manual]\n";

  Random.self_init ();
  if !opts.test then (
    if !opts.manual then
      Test.run_manual ~dict:!opts.dict
    else
      Test.run_suite ~dict:!opts.dict ~log:true
  ) else (
    play_game !opts
  );;

main ();
