(*
  Copyright 2014 Robert Moss.
  Distributed under the BSD 3-Clause license (see LICENSE).
*)

(** Logic and solvers for playing the different Countdown rounds. *)

(** Dictionaries that support searching for longest words. *)
module Dictionary = struct

  (** {2 Type definitions} *)

  (* Words are stored in a trie. *)

  (** A node in the trie. *)
  type node = {
    ch: char;
    (** The character to append to the node's prefix. *)
    word: bool;
    (** Whether or not this node represents a complete word. *)
    children: node list;
    (** The children nodes (i.e., longer words). *)
  }

  (** Rather than explicitly representing the empty string as the root node
      for the trie, a dictionary is stored as a list of the immediate children
      (i.e., the first character of each word). *)
  type t = node list

  (** {2 String conversions} *)

  (** Convert a string to a list of characters. *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  (** Convert a list of characters to a string. *)
  let implode l =
    let res = String.create (List.length l) in
    let rec imp i = function
      | [] -> res
      | c :: l -> res.[i] <- c; imp (i + 1) l in
    imp 0 l

  (** {2 Initialisation} *)

  (** For simplicity, ignore words containing capital letters (on the basis
      that they are proper nouns or acronyms) and accented characters (on the
      basis that the non-accented version also should be an acceptable English
      word; e.g., "naive"). *)
  let is_valid chars =
    match chars with
    | [] -> false
    | _ -> List.for_all (fun ch -> 'a' <= ch && ch <= 'z') chars

  (** Insert a sequence of characters into a dictionary. *)
  let rec insert chars nodes =
    match chars with
    | [] -> nodes
    | c::cs ->
      let word = cs = [] in
      let rec insert_head ns =
        match ns with
        | [] ->
          [{ ch = c; word = word; children = insert cs [] }]
        | n::ns ->
          if (n.ch = c) then
            let word = n.word || word in
            let n = { n with word = word; children = insert cs n.children } in
            n :: ns
          else
            n :: (insert_head ns)
      in
      insert_head nodes

  (** Add a word to the dictionary, if it is deemed to be valid. *)
  let add_word dict chars =
    if (is_valid chars) then (
      insert chars dict
    ) else (
      dict
    )

  (** Load a dictionary from an input channel. *)
  let from_channel channel =
    let get_chars ch =
      try
        let line = input_line ch in
        Some (explode line)
      with End_of_file ->
        None
    in
    let rec parse_line dict =
      let chars = get_chars channel in
      match chars with
      | None -> dict
      | Some cs ->
        let dict = add_word dict cs in
        parse_line dict
    in
    let dict = [] in
    parse_line dict

  (** Load a dictionary from a file. *)
  let from_file filename =
    let ch = open_in filename in
    let dict = from_channel ch in
    close_in ch;
    dict

  (** {2 Inspection} *)

  (** Print all words in a dictionary to an output channel. *)
  let print dict ch =
    let rec print ?(level=0) dict =
      let indent = String.make (level * 4) ' ' in
      match dict with
      | [] -> ()
      | n::ns ->
        if (n.word) then (
          Printf.fprintf ch "%s'%c' *\n" indent n.ch
        ) else (
          Printf.fprintf ch "%s'%c'\n" indent n.ch
        );
        print ~level:(level + 1) n.children;
        print ~level ns
    in
    print dict

  (** {2 Filtering} *)

  (** Only retain words that can be spelt using the given list of characters
      (multiple uses of a character are forbidden). *)
  let rec filter dict chars =
    match chars with
    | [] -> []
    | _ ->
      let rec contains chars node =
        match chars with
        | [] -> false
        | c::cs ->
          node.ch = c || contains cs node
      in
      let rec remove ch chars =
        match chars with
        | [] -> []
        | c::cs ->
          if (c = ch) then cs else c :: (remove ch cs)
      in
      let filter_nodes accum node =
        if (contains chars node) then (
          let remaining = remove node.ch chars in
          let children = filter node.children remaining in
          match children with
          | [] ->
            if (node.word) then (
              { node with children = []; } :: accum
            ) else (
              accum
            )
          | _ ->
            { node with children = children; } :: accum
        ) else
          accum
      in
      List.fold_left filter_nodes [] dict

  (** {2 Searching} *)

  (** Return the longest word(s) in the dictionary. *)
  let longest dict =
    let keep_longest word_list =
      let cmp (alen, _) (blen, a) = blen - alen in
      let sorted = List.sort cmp word_list in
      let rec keep len words =
        match words with
        | [] -> []
        | (wlen, word) :: ws ->
          if (wlen = len) then
            (wlen, word) :: keep len ws
          else
            []
      in
      match sorted with
      | [] -> []
      | (len, _) :: ws ->
        keep len sorted
    in
    (* Find all of the leaf nodes and keep the longest! *)
    (* So, for each child return the length of the longest *and* the word *)
    let rec find_words node prefix length =
      match node.children with
      | [] -> [length + 1, List.rev (node.ch :: prefix)]
      | _ ->
        let rec child_words child =
          let words = find_words child (node.ch :: prefix) (length + 1) in
          keep_longest words
        in
        List.flatten (List.map child_words node.children)
    in
    let words = List.map (fun n -> find_words n [] 0) dict in
    let words = keep_longest (List.flatten words) in
    List.map (fun (l,cs) -> implode cs) words

  (** Return the longest word(s) in the dictionary that are spelt using only
      the given list of characters (multiple uses are forbidden). *)
  let find_longest dict chars =
    longest (filter dict chars)

  (** Return the word(s) in the dictionary that match the given pattern, where
      each character is either a specific letter ('a'..'z') or unknown ('?'). *)
  let match_pattern dict chars ?using () =
    (** Determine whether a node matches a blank, given a list of letters that
        can be used to match a blank. *)
    let consume_letter substr node letters =
      let rec find_letter ls accum =
        match ls with
        | [] -> (false, (substr, node, Some letters))
        | l::ls ->
          if l == node.ch then
            (true, (node.ch::substr, node, Some (accum @ ls)))
          else
            find_letter ls (l::accum)
      in
      find_letter letters []
    in
    (** Determine if this node matches the current character.
        It returns a tuple (match, (substr, node, letters)), where match is a
        boolean than indicates whether the node is a match, substr is the
        string that has been built so far *including this node*, and letters
        is the list of remaining letters (if any) that can match a blank. *)
    let is_match char (substr, node, ls) =
      match ls with
      | None ->
        (* Any letter can match a blank. *)
        if char == '?' || node.ch == char then
          (true, (node.ch::substr, node, ls))
        else
          (false, (substr, node, ls))
      | Some letters ->
        (* Only specific letters can match a blank. *)
        if node.ch == char then
          (true, (node.ch::substr, node, ls))
        else if char == '?' then
          (* Check if the node matches one of these letters and, if so,
             remove this letter from the list. *)
          consume_letter substr node letters
        else
          (false, (substr, node, ls))
    and children char (substr, node, ls) =
      List.map (fun n -> (substr, n, ls)) node.children
    in
    let rec step_down partials ch rest =
      (* Determine which nodes match the current character in the pattern. *)
      let partials = List.map (is_match ch) partials in
      (* Discard non-matching nodes. *)
      let partials = List.filter (fun (bool, _p) -> bool) partials in
      let partials = List.map (fun (_bool, p) -> p) partials in
      match rest with
      | [] ->
         (* This was the last character, return all matching strings that are
            identified as being whole words. *)
         let words = List.filter (fun (_, n, _ls) -> n.word) partials in
         List.map (fun (substr, n, _ls) -> implode (List.rev (substr))) words
      | r::rs ->
         (* There are subsequent characters to match against the children of
            every matching node. *)
         let partials = List.flatten (List.map (children ch) partials) in
         step_down partials r rs
    in
    match chars with
    | [] -> [] (* No words of length zero! *)
    | c::cs -> step_down (List.map (fun n -> ([], n, using)) dict) c cs
end

(** Random selection of numbers and letters. *)
module AtRandom = struct

  (** Generate a population from a frequency list. *)
  let popn_of_freqs dist =
    let rec add_choices accum dist =
      match dist with
      | [] -> List.rev accum
      | (v, n)::ds ->
        let head = Array.make n v in
        add_choices (head :: accum) ds
    in
    Array.to_list (Array.concat (add_choices [] dist))

  (** Randomly select an element from a population.
      Return both the selected value and the remaining population.
      The pseudo-random number generator ([Random]) should be initialised
      {b before} calling this function. *)
  let choice choices =
    let len = List.length choices in
    let ix = Random.int len in
    let value = List.nth choices ix in
    let new_choices =
      let rec remove value choices =
        match choices with
        | [] -> []
        | c::cs ->
          if (c = value) then cs else c :: (remove value cs)
      in
      remove value choices
    in
    value, new_choices

  (** Randomly select an integer within the given (inclusive) bounds.
      The pseudo-random number generator ([Random]) should be initialised
      {b before} calling this function. *)
  let int ~min ~max =
    let range = max - min in
    let value = Random.int (range + 1) in
    value + min

end

(** Logic for the Letters round. *)
module Letters = struct

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
  type t = {
    dictionary: Dictionary.t;
    (** The list of valid words. *)
    count: int;
    (** How many letters must be selected for a single round. *)
    max_vs: int;
    (** The maximum number of vowels that can be selected. *)
    max_cs: int;
    (** The maximum number of consonants that can be selected. *)
    num_vs: int;
    (** The number of vowels that have been selected. *)
    num_cs: int;
    (** The number of consonants that have been selected. *)
    letters: char list;
    (** The list of selected letters. *)
    vs: char list;
    (** The list of available vowels. *)
    cs: char list;
    (** The list of available consonants. *)
  }

  (** {2 Initialisation} *)

  (* The distribution of vowels (as used in Scrabble). *)
  let freq_vowels =
    [ 'e', 12; 'a', 9; 'i', 9; 'o', 8; 'u', 4; ]

  (* The distribution of consonants (as used in Scrabble). *)
  let freq_consonants =
    [ 'n', 6; 'r', 6; 't', 6; 'l', 4; 's', 4; 'd', 4; 'g', 3; 'b', 2; 'c', 2;
      'm', 2; 'p', 2; 'f', 2; 'h', 2; 'v', 2; 'w', 2; 'y', 2; 'k', 1; 'j', 1;
      'x', 1; 'q', 1; 'z', 1;]

  (** The default options, which describe the standard Letters game. *)
  let config ~dict_file =
    let dict = Dictionary.from_file dict_file
    and vs = AtRandom.popn_of_freqs freq_vowels
    and cs = AtRandom.popn_of_freqs freq_consonants in
    { dict = dict;
      vowels = vs;
      consonants = cs;
      per_round = 9;
      max_vowels = 5;
      max_consonants = 6;
    }

  (** Start a new round of the Letters game. *)
  let init config = {
    dictionary = config.dict;
    count = config.per_round;
    max_vs = config.max_vowels;
    max_cs = config.max_consonants;
    num_vs = 0;
    num_cs = 0;
    letters = [];
    vs = config.vowels;
    cs = config.consonants;
  }

  (** {2 Game state} *)

  (** How many letters remain to be selected. *)
  let remaining t =
    t.count - (List.length t.letters)

  (** The letters that have been selected so far. *)
  let letters t =
    List.rev t.letters

  (** Whether the player can select any more vowels. *)
  let more_vowels t =
    t.max_vs > t.num_vs

  (** Whether the player can select any more consonants. *)
  let more_consonants t =
    t.max_cs > t.num_cs

  (** {2 Game actions} *)

  (** Determine whether or not a letter is a vowel. *)
  let is_vowel c =
    List.exists (fun (ch, _) -> ch = c) freq_vowels

  (** Determine whether or not a letter is a consonant. *)
  let is_consonant c =
    List.exists (fun (ch, _) -> ch = c) freq_consonants

  (** Add a specific letter to the list of selected letters. *)
  let add_letter t ch =
    let num_vs, num_cs =
      match is_vowel ch, is_consonant ch with
      | true, false -> t.num_vs + 1, t.num_cs
      | false, true -> t.num_vs, t.num_cs + 1
      | _ -> failwith (Printf.sprintf "ERROR: invalid letter '%c'" ch)
    in
    { t with
      num_vs = num_vs;
      num_cs = num_cs;
      letters = ch :: t.letters; }

  (** Randomly select a vowel, using {!AtRandom.choice}. *)
  let add_random_vowel t =
    let ch, vs = AtRandom.choice t.vs in
    ch, { t with vs = vs; num_vs = t.num_vs + 1; letters = ch :: t.letters; }

  (** Randomly select a consonant, using {!AtRandom.choice}. *)
  let add_random_consonant t =
    let ch, cs = AtRandom.choice t.cs in
    ch, { t with cs = cs; num_cs = t.num_cs + 1; letters = ch :: t.letters; }

  (** {2 Solutions} *)

  (** Return a list of the longest words that can be spelt using the selected
      letters for the round. *)
  let solve state =
    Dictionary.find_longest state.dictionary state.letters

end

(** Logic for the Numbers round. *)
module Numbers = struct

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
  type t = {
    count: int;
    max_ls: int;
    max_ss: int;
    num_ls: int;
    num_ss: int;
    numbers: int list;
    ls: int list;
    ss: int list;
    target: int;
  }

  (** Equations that can be generated from a given set of numbers. *)
  type eq =
  | Num of int
  (** A number. *)
  | Add of (eq * eq)
  (** The addition of two numbers. *)
  | Sub of (eq * eq)
  (** The subtraction of one number from another, necessarily positive. *)
  | Mul of (eq * eq)
  (** The product of two numbers. *)
  | Div of (eq * eq)
  (** The quotient  of two numbers, given that the remainder is zero. *)

  (** {2 Initialisation} *)

  (** The distribution of large numbers: one of each. *)
  let freq_large =
    [ 25, 1; 50, 1; 75, 1; 100, 1; ]

  (** The distribution of small numbers: two of each. *)
  let freq_small =
    [ 1, 2; 2, 2; 3, 2; 4, 2; 5, 2; 6, 2; 7, 2; 8, 2; 9, 2; 10, 2; ]

  (** The default options, which describe the standard Numbers game. *)
  let config = {
    large = AtRandom.popn_of_freqs freq_large;
    small = AtRandom.popn_of_freqs freq_small;
    per_round = 6;
    max_large = 4;
    max_small = 6; }

  (** Start a new round of the Numbers game, with a target chosen at random
      using {!AtRandom.choice}. *)
  let init config = {
    count = config.per_round;
    max_ls = config.max_large;
    max_ss = config.max_small;
    num_ls = 0;
    num_ss = 0;
    numbers = [];
    ls = config.large;
    ss = config.small;
    target = AtRandom.int ~min:100 ~max:999 ; }

  (** {2 Game state} *)

  (** How many numbers remain to be selected. *)
  let remaining t =
    t.count - (List.length t.numbers)

  (** The numbers that have been selected so far. *)
  let numbers state =
    List.rev state.numbers

  (** Whether the player can select any more large numbers. *)
  let more_large t =
    t.max_ls > t.num_ls

  (** Whether the player can select any more small numbers. *)
  let more_small t =
    t.max_ss > t.num_ss

  (** The target value. *)
  let target t =
    t.target

  (** {2 Game actions} *)

  (** Determine whether or not a number is small. *)
  let is_small n =
    List.exists (fun (x, _) -> x = n) freq_small

  (** Determine whether or not a number is large. *)
  let is_large n =
    List.exists (fun (x, _) -> x = n) freq_large

  (** Add a specific number to the list of selected numbers. *)
  let add_number t n =
    let num_ss, num_ls =
      match is_small n, is_large n with
      | true, false -> t.num_ss + 1, t.num_ls
      | false, true -> t.num_ss, t.num_ls + 1
      | _ -> failwith (Printf.sprintf "ERROR: invalid letter '%d'" n)
    in
    { t with
      num_ss = num_ss;
      num_ls = num_ls;
      numbers = n :: t.numbers; }

  (** Randomly select a large number, using {!AtRandom.choice}. *)
  let add_random_large t =
    let n, ls = AtRandom.choice t.ls in
    n, { t with ls = ls; num_ls = t.num_ls + 1; numbers = n :: t.numbers; }

  (** Randomly select a small number, using {!AtRandom.choice}. *)
  let add_random_small t =
    let n, ss = AtRandom.choice t.ss in
    n, { t with ss = ss; num_ss = t.num_ss + 1; numbers = n :: t.numbers; }

  (** Override the target value. *)
  let set_target t target =
    { t with target = target; }

  (** {2 Solutions} *)

  (** Count how many numbers appear in an equation. *)
  let rec eq_count eq =
    match eq with
    | Num _ -> 1
    | Add (eq1, eq2) -> (eq_count eq1) + (eq_count eq2)
    | Mul (eq1, eq2) -> (eq_count eq1) + (eq_count eq2)
    | Sub (eq1, eq2) -> (eq_count eq1) + (eq_count eq2)
    | Div (eq1, eq2) -> (eq_count eq1) + (eq_count eq2)

  (** Return a string representation of an equation. *)
  let string_of_eq eq =
    let b = Buffer.create 40 in
    let rec add_eq ?(initial=false) eq =
      let eq_string ?(brackets=true) symbol eq1 eq2 =
        let brackets = brackets && not initial in
        if brackets then Buffer.add_char b '(';
        add_eq eq1;
        Buffer.add_char b ' ';
        Buffer.add_string b symbol;
        Buffer.add_char b ' ';
        add_eq eq2;
        if brackets then Buffer.add_char b ')';
      in
      match eq with
      | Num x -> Buffer.add_string b (string_of_int x)
      | Add (eq1, eq2) -> eq_string "+" eq1 eq2
      | Sub (eq1, eq2) -> eq_string "-" eq1 eq2
      | Mul (eq1, eq2) -> eq_string ~brackets:false "*" eq1 eq2
      | Div (eq1, eq2) -> eq_string ~brackets:false "/" eq1 eq2
    in
    add_eq ~initial:true eq;
    Buffer.contents b

  (** Remove the first occurrence of a value from a list. *)
  let rec remove x ys =
    match ys with
    | [] -> []
    | y::ys ->
      if x = y then ys else y :: (remove x ys)

  (** Return tuples [(a, b, ys)] for every unordered pair [(a,b)] in [xs],
      where [ys] contains every element of [xs] except [a] and [b]. *)
  let pairs xs =
    let rec elt_pairs accum elts =
      match elts with
      | [] -> List.flatten (List.rev accum)
      | e::[] -> List.flatten (List.rev accum)
      | e::es ->
      (* Combine all pairs (e,_) with all of the selected numbers except e and
         the other element of each pair. *)
        let not_e = remove e xs in
      (* Only consider pairs involving the current number (e) and numbers that
         appear later in the list (es).
         This avoids producing duplicate pairs (a,b) and (b,a). *)
        let new_pairs = List.map (fun x -> e, x, remove x not_e) es in
        elt_pairs (new_pairs :: accum) es
    in
    elt_pairs [] xs

  (** Return a list of every valid arithmetic combination of two numbers. *)
  let apply_ops ((x, x_op), (y, y_op), rest) =
  (* The addition and product of two positive integers are always positive
     integers, and so are always valid operations. *)
    let ops = [
      ((x + y), Add (x_op, y_op)) :: rest;
      ((x * y), Mul (x_op, y_op)) :: rest; ]
    in
    let ops =
    (* The subtraction of a smaller number from a larger number is a valid
       operation. *)
      if (x > y) then
        ((x - y, Sub (x_op, y_op)) :: rest) :: ops
      else if (x < y) then
        ((y - x, Sub (y_op, x_op)) :: rest) :: ops
      else
        ops
    in
  (* The quotient of two numbers is a valid operation if, and only if, the
     remainder is zero. *)
    if (x mod y = 0 && y > 1) then
      ((x / y, Div (x_op, y_op)) :: rest) :: ops
    else if (y mod x = 0 && x > 1) then
      ((y / x, Div (y_op, x_op)) :: rest) :: ops
    else
      ops

  (** Return an optimal solution [(value, formula)] for the round. *)
  let solve state =
    (* For every pair of numbers, replace the pair by the result of each valid
       arithmetic operation.
       In this way, each list of remaining numbers will contain one less
       number than the original list.
       Apply this recursively to generate all possible values that can be
       generated from the selected numbers.

       Once an exact solution is obtained, it can only be improved by finding
       an exact solution that uses fewer numbers.
       This allows us to greatly reduce the search space, once an exact
       solution is found.

       Consider the case X = 1 (i.e., target = Num A).
       We no longer need to search lists of N or fewer numbers.
       Consider the case X = 2 (e.g., target = Add (Num A) (Num B).
       We no longer need to search lists of N - 1 or fewer numbers.

       Given an exact solution that uses X variables, we only need to keep
       searching if there are more than (N + 1 - X) numbers remaining.
       So, if there are n numbers remaining, we can stop searching if:

         n <= N + 1 - X    or, alternatively    X <= N + 1 - n
    *)

    (* Compare the current best solution against the list of numbers that have
       been produced by applying a single arithmetic operation to the list of
       remaining numbers. *)
    let best_of curr new_nums =
      let choices = curr :: new_nums in
      (* The absolute difference between each number and the target. *)
      let diffs =
        List.map (fun (x, xop) -> abs(x - state.target), x, xop) choices
      in
      (* Sort from smallest to largest absolute difference. *)
      let sorted =
        let cmp (xd, x, xop) (yd, y, yop) =
          let r = compare xd yd in
          if r = 0 then
            (* If the solutions are the same, prefer the equation that uses
               the fewest numbers. *)
            compare (eq_count xop) (eq_count yop)
          else
            r
        in
        List.sort cmp diffs
      in
      (* Return the best solution as defined by cmp, above. *)
      match List.hd sorted with
      | (diff, value, op) -> value, op
    in

    (* Consider all valid arithmetic operations on every pair of numbers, and
       compare the results to the current solution. *)
    let rec combine best numbers =
      let all_pairs = pairs numbers in
      let new_numbers = List.flatten (List.map apply_ops all_pairs) in
      let best = best_of best (List.flatten new_numbers) in

      (* Whether or not we can stop searching. *)
      let prune nvars nleft =
        nvars <= state.count + 1 - nleft
      in

      let nums_left = List.length (List.hd new_numbers)
      and exact_soln = fst best = state.target
      and soln_vars = eq_count (snd best) in

      if nums_left < 2 then (
        (* There are no more pairs of numbers to combine. *)
        best
      ) else if exact_soln && prune soln_vars nums_left then (
        (* We have an exact solution, shorter than any we could generate by
           continuing to search. *)
        best
      ) else (
        (* Generate more combinations with the new numbers. *)
        let rec next_level best ns =
          match ns with
          | [] -> best
          | n::ns ->
            let nbest = combine best n in
            (* Decide whether to keep searching.
               Stop if we have an exact solution that uses no more variables
               than the fewest possible if we keep searching. *)
            let nleft = nums_left - 1
            and nexact = fst nbest = state.target
            and nvars = eq_count (snd nbest) in
            if nexact && prune nvars nleft then (
              nbest
            ) else (
              next_level nbest ns
            )
        in
        next_level best new_numbers
      )
    in

    let numbers = List.map (fun x -> x, Num x) state.numbers in
    (* Select the number that is closest to the target value as the initial
       best-case solution. *)
    let best = best_of (0, Num 0) numbers in
    let value, eq = combine best numbers in
    value, string_of_eq eq

end
