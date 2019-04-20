---
layout: post
title: A lazy implementation of KMP in OCaml
---
The following is a write-up on an implementation of the [Knuth-Morris-Pratt]
(or [KMP]) text search algorithm in OCaml.  The algorithm itself is rather
straight forward but implementing it in a functional style serves as a good
example of how lazy data structures may be used as an optimization technique.

## The algorithm

[KMP] solves the problem of efficiently searching for occurrences of one
string within another.

To illustrate, searching for the string `ABCD` in `--ABC-ABCF-ABCD--ABCDEF`
should recognize two matches at position *12* and *18*, counting from one.

Efficiency is achieved by realizing that backtracking, after matching
a prefix of the search pattern, may be eliminated if the pattern is analyzed upfront.

For instance searching for the above pattern from left to right, the sequence of the first
three characters (`A`, `B` and `C`) matches the initial characters of the string being
searched; When realizing that the match fails at the fourth character, a naive
implementation would shift back two characters, starting the search from scratch at
`B`.

By pre examining the search pattern, it is clear that backtracking in this
case may be safely omitted since no previous sequence of characters is a
prefix of the pattern. In other words, the only reason for ever backtracking
`k` steps after failing to match after `n` steps into the pattern, is if the
sequence of the pattern starting at `n - k` and ending at `n` is a prefix of
the pattern itself! Thus the [KMP] algorithm contains an initial step for
generating a lookup table where each index of the search pattern is mapped to
a corresponding number of steps required for backtracking. The table for the
search string: `ABCD AB ABC DEF` looks like:

```ocaml
ABCD AB ABC DEF
000000120123000
```

Here the first character for which backtracking is required is the second
occurrence of `B` (position 7); That is because the previous character is `A`,
which may indeed be a starting point of a complete match.  Even in this case
however, backtracking would be redundant since the alternative prefix match is
already known; One may simply continue to match the next character of the
string being searched, while shifting the pattern itself.  This ensures a
strict linear bound on the complexity of the algorithm after the initial
lookup table has been constructed.


## Implementation

The lookup table from above precisely defines the type of shifting required
for each sequence of matches of the input pattern. As such, the table is
really an encoding of a state machine where a transition between two steps can
be decided exclusively by looking at the result of matching the next
character. Such an automata can be represented in OCaml using the following
type:

```ocaml
type pattern = { is_match : bool; step: char -> pattern }
```

The boolean flag indicates whether the pattern is in a matching state or not,
that is if a complete match of the pattern has been observed.  The function
step defines the transition. For simplicity I'm using `char` here although
there is little point in constraining the type.

Using this definition of pattern, it is straight forward to
write a transformation turning a pattern into a search function:

```ocaml
let run (pattern: pattern) (text: string) : int list =
  let accum (pattern,ix,acc) c =
    let pattern = next c pattern in
    let acc = if is_match pattern then (ix :: acc) else acc in
    (pattern, ix + 1, acc)
  in
  let (_,_,acc) = List.fold_left accum (pattern, 0, []) text in
  List.rev acc
```

The function `run` accepts a pattern and returns a function than can be
applied on any string, yielding a list of positions.

More interesting is the generation of the pattern itself. How can we define
define a function that when given a list of characters returns a pattern
representing the table described by [KMP]?

Looking at the structure of the table, it's clear that the state machine it
encodes contains cycles. That is, failing to match at one state means
transitioning to a prior state; Most of the times the initial state.  Cyclic
structures and functional programming may sound like at odds, although a
cyclic structure are semantically equivalent with infinite trees.  In this
case however, we better be careful; The whole purpose of [KMP] is to avoid
redundant re-computations why we need to make sure that each step of the
automata is generated once and once only.

Logically constructing the pattern corresponding to the [KMP] table is simple.
At each character of the input search string, a step is generated; Unless the
input string is empty the result is an incomplete state with the `is_match`
property set to `false`. The step function looks at an incoming character and
decide to continue to the next step or return to a previous step depending on
whether the character matches the current character of the pattern or not.

The challenge is to figure out how to refer to the previous state. Applying
the following thought experiment might help; Pretend for a second that an
already have a completed pattern automata, `p0` is given, with its current
state set to the initial one. One could replicate the pattern by
looking at the input from left two right, using `p0` as a reference for
backtracking in case of failed matches. Consider the example above with input
pattern:

```ocaml
ABCD AB ABC DEF
```

Starting by matching `p0` at the second character, that is feeding it
with `B`, would bring it back to its initial state expecting an `A`. Next
step, given `C` and again it would reset itself. A few more characters into
the sequence, matching `A` and `B` it would progress two steps where next step
expects `C`.  After eight steps a space character is expected and `p0` has now
matched `A` and `B` and thus expects a `C`.  When given `p0`, the step
function is simple to define: If a space character is provided continue to the
next step, otherwise go to `p0`.

Technically this approach poses a problem - In order to generate the pattern
we need the pattern! Sounds like being trapped in a catch twenty two
situation.  However, examining the problem carefully should make it clear that
it is not necessary to peek inside `p0` until at least one step has been
constructed; So instead of passing a value that is fully evaluated one might
just get away by passing a lazy value and be careful not to force its
evaluation until after it's created.  The `lazy` keyword in OCaml provides
such a mechanism.

Here is the complete implementation of pattern generation function
implementing the described strategy:

```ocaml
open Lazy
let mk b f = { is_match = b ; step = f }
let step c p = p.step c
let rec const b = { is_match = b; step = fun _ -> const b }

let generate_pattern (cs: char list) : pattern =
  let rec pattern = lazy (gen pattern cs)
  and gen curr_pattern = function
    | []                ->
      const true
    | [c]               ->
      mk false @@ fun x ->
        let next_pattern = force curr_pattern in
        if x = c then
          mk true (fun _ -> next_pattern)
        else
          next_pattern
    | c :: cs           ->
      let next_pattern = lazy (step c @@ force curr_pattern) in
      let cont_pattern = lazy (gen next_pattern cs) in
      mk false @@ fun x ->
        force @@ if x = c then cont_pattern else curr_pattern
  in
  force pattern
```

The one line that may stick out is the recursive definition of pattern: `let
rec pattern = lazy (gen pattern cs)`.  The operator `lazy` creates an
unevaluated *thunk* which enables forward referencing.

The function `gen` is the work horse and its parameter `curr_pattern`
corresponds to `p0` from above, and is a handler to a lazy pattern.

The function matches a list of characters constituting the given pattern and
differentiates between three cases; An empty list is a special case and will
result in  a pattern that always matches and resets itself to the same state.
For a single character, a step is constructed which when given a character,
forces evaluation of `curr_pattern` and branches depending on the match.

When more than one character is matched, the succeeding step is constructed
lazily by applying the first character to the current pattern and recursively
generating the continuation pattern. Note that the evaluation of
`cont_pattern` is only forced from within the step function.

It is important to realize that he automata generated by the function is indeed cyclic.

For completeness, here's the final search function composing `run` and `generate_pattern`:

```ocaml
let search cs = run @@ generate_pattern cs
```

Perhaps a bit mind boggling (at least for me) but I hope this example helps to
showcase how *lazy data structures* may be used as an optimization technique,
at least providing an alternative to more imperative approaches such as
mutable arrays.

Thanks to [Shayne Fletcher] for coming up with the challenge in the first place.

[KMP]:https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
[Knuth-Morris-Pratt]:https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
[Shayne Fletcher]:http://blog.shaynefletcher.org/

