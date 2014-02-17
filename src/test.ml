(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Test cases for the Countdown rounds. *)

module Letters = Countdown.Letters
module Numbers = Countdown.Numbers

(** {2 Type definitions} *)

(** Test cases for the Letters and Numbers rounds. *)
type t =
| Letters of string * string
(** Characters, solution. *)
| Numbers of int list * int * int * string option
(** Numbers, target, best answer, equation (optional). *)

(** {2 Testing routines} *)

(** Print all solutions to the Letters round and highlight the expected
    solution (if it occurs). *)
let string_of_solution word soln =
  let b = Buffer.create (13 * List.length soln) in
  let add_word ?(last=false) w =
    let quote = if w = word then "*" else "'" in
    Buffer.add_string b quote;
    Buffer.add_string b w;
    Buffer.add_string b quote;
    if not last then Buffer.add_string b ", "
  in
  let rec add_words words =
    match words with
    | [] -> ()
    | [w] -> add_word ~last:true w
    | w::ws ->
      add_word w;
      add_words ws
  in
  add_words soln;
  Buffer.contents b

let test_letters_soln ~state ~ls ~word ~log =
  let soln = Letters.solve state in
  let s = string_of_solution word soln in
  let success = List.mem word soln in
  if log then (
    if success then
      Printf.printf "SUCCESS letters='%s' words=%s\n" ls s
    else
      Printf.printf "FAILURE letters='%s' expect='%s' words=%s\n" ls word s
  );
  success

(** Test whether the Letters round produces the same [word] as a valid
    solution for the letters [ls]. *)
let test_letters ~cfg ~ls ~word ~log =
  let state = Letters.init cfg
  and chars = Countdown.Dictionary.explode ls in
  let state = List.fold_left Letters.add_letter state chars in
  test_letters_soln ~state ~ls ~word ~log

let test_numbers_soln ~state ~target ~value ~eqn ~log =
  let v, eq_str = Numbers.solve state in
  let eq_match =
    match eqn with
    | None -> true
    | Some str -> str = eq_str
  in
  let success = v = value && eq_match in
  if log then (
    if success then
      Printf.printf "SUCCESS target=%d value=%d eq='%s'\n" target v eq_str
    else (
      match eqn with
      | None ->
        Printf.printf "FAILURE target=%d expect=%d value=%d eq='%s'\n"
          target value v eq_str
      | Some str ->
        Printf.printf "FAILURE target=%d value=%d eq='%s' expect='%s'\n"
          target v eq_str str
    )
  );
  success

(** Test whether the Numbers round produces the same [value] when using the
    [numbers] to get as close as possible to the [target].
    The equation [eqn] that produces this [value] may also be given. *)
let test_numbers ~cfg ~ns ~target ~value ~eqn ~log =
  let state = Numbers.(List.fold_left add_number (init cfg) ns) in
  let state = Numbers.set_target state target in
  test_numbers_soln ~state ~target ~value ~eqn ~log

(** {2 Manual testing} *)

(** Play the Letters round, allowing the user to select the letters and an
    expected solution. *)
let test_letters_manual config =
  let rec add_letters state =
    let rem = Letters.remaining state in
    if rem = 0 then (
      state
    ) else (
      Printf.printf "Enter the letters (%d remaining): " rem;
      let chars = Countdown.Dictionary.explode (read_line ()) in
      let rec add_chars state chars =
        match chars with
        | [] -> state
        | c::cs ->
          if Letters.remaining state > 0 then
            add_chars (Letters.add_letter state c) cs
          else
            state
      in
      let state = add_chars state chars in
      add_letters state
    )
  in
  let state = add_letters (Letters.init config) in
  Printf.printf "Enter a solution: ";
  let word = read_line () in
  let ls = Countdown.Dictionary.implode (Letters.letters state) in
  ignore (test_letters_soln ~state ~ls ~word ~log:true)

(** Play the Numbers round, allowing the user to select the numbers, the
    target, and the expected solution. *)
let test_numbers_manual config =
  let rec add_numbers state =
    let rem = Numbers.remaining state in
    if rem = 0 then (
      state
    ) else (
      Printf.printf "Enter a number (%d remaining): " rem;
      let num = read_int () in
      add_numbers (Numbers.add_number state num)
    )
  in
  let state = add_numbers (Numbers.init config) in
  Printf.printf "Enter the target: ";
  let target = read_int () in
  Printf.printf "Enter the expected solution: ";
  let state = Numbers.set_target state target in
  let value = read_int () in
  ignore (test_numbers_soln ~state ~target ~value ~eqn:None ~log:true)

(** Test whether a Letters or Numbers round produces the expected solution. *)
let run ~letters ~numbers ~log test =
  let success =
    match test with
    | Letters (ls, word) ->
      test_letters ~cfg:letters ~ls ~word ~log
    | Numbers (ns, target, value, eqn) ->
      test_numbers ~cfg:numbers ~ns ~target ~value ~eqn ~log
  in
  if log then flush stdout;
  success

(** {2 Test cases} *)

(** A suite of test cases for both the Letters and Numbers rounds. *)
let suite = [
  Letters ("symphonyz", "symphony");
  Letters ("symphonyz", "hyponyms");
  Letters ("auodfckeg", "dogface");
  Letters ("auodfckeg", "dockage");
  Letters ("gyhdnoeur", "greyhound");
  Letters ("chinalung", "launching");
  Letters ("apexjoust", "juxtapose");
  Letters ("anglehoax", "hexagonal");
  Letters ("curlidied", "ridiculed");
  Letters ("nicegroan", "ignorance");
  Letters ("teresacry", "secretary");
  Letters ("oxonshape", "saxophone");
  Letters ("dragafuse", "safeguard");
  Letters ("evildread", "daredevil");
  Letters ("archenimy", "machinery");
  Letters ("amusingmy", "gymnasium");
  Numbers ([1;2;3;4;50;100;], 100, 100, Some "100");
  Numbers ([1;2;3;4;50;75;], 125, 125, None);
  Numbers ([2;3;4;5;25;50;], 261, 261, None);
  Numbers ([2;3;4;5;25;50;], 989, 989, None);
  Numbers ([2;2;4;1;3;1;], 959, 108, Some "(1 + 2) * (1 + 2) * 3 * 4");
  Numbers ([50;25;10;9;6;5;], 712, 711, Some "(6 + 10) * (50 - 5) - 9");
]

(** Run the suite of test cases. *)
let run_suite ~dict ~log =
  let letters = Letters.config dict
  and numbers = Numbers.config in
  List.iter (fun t -> ignore (run ~letters ~numbers ~log t)) suite

(** Run manual test cases that prompt the user for input. *)
let run_manual ~dict =
  let letters = Letters.config dict
  and numbers = Numbers.config in

  let continue = ref true in
  let rec test_round () =
    let msg = "\nPlay a round of letters, numbers, or quit [lnq]: "
    and choices = [
      "l", (fun _ -> test_letters_manual letters);
      "n", (fun _ -> test_numbers_manual numbers);
      "q", (fun _ -> continue := false); ]
    in
    let test_fun = CLI.Console.prompt ~msg ~choices in
    test_fun ();
    if !continue then test_round ()
  in

  test_round ()
