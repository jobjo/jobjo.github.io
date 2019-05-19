---
layout: post
title: Applicative parsing
---

[Parser combinators](https://en.wikipedia.org/wiki/Parser_combinator) are
sets of functions for building parsers in a composable fashion. Haskell's
[Parsec library](http://hackage.haskell.org/package/parsec) and OCaml's
[Angstrom](https://github.com/inhabitedtype/angstrom) are two examples.
Both of these libraries expose *monadic* interfaces for
describing context-sensitive grammars. This post looks at implementing a more
restricted parsing library, structured around [applicative
functors](https://en.wikipedia.org/wiki/Applicative_functor) rather
than monads.

What could justify giving up on monads? Depending on the design, one may get
a few things in return. In this exercise, the aim is for an API with the following
features:

  1. The ability to extract all valid symbols from a parser.
  2. Allow some form of pretty printing.
  3. Support multiple evaluation strategies (e.g. backtracking and non-backtracking).

To see why (1) is not possible to accomplish with monads, consider a parser
constructed using the monadic *bind-operator*:

```ocaml
let p = p1 >>= f
```

Here, `f` is a function of the form `'a -> 'b parser`; that means we don't
know what sort of parser it produces until it's provided with a value. Therefore,
we cannot infer all possible symbols consumed by those parsers. The same
reasoning applies to pretty printing.

### Designing a parser type

The essence of a parser is a function with a type analogous to:

```ocaml
char list -> ('a * char list) option
```

A parser takes a list of characters as input, and, when successful, returns a
parsed value along with the remaining input.

This representation, however, falls short of supporting symbol
extraction, pretty printing, or allowing for multiple evaluation strategies.
To accommodate for those we'd have to embellish the type with more context.
The problem is we don't necessarily know the complete feature set upfront.
Every time a request for some new capability comes in -- be it error reporting,
logging or something else -- we would have to go back and change the definition
to accommodate for the new functionality.

Rather than trying to anticipate all use cases upfront, an alternative
approach is to choose a representation that preserves as much structure as
possible, so that alternative interpreters may be added later on. To do that,
we're effectively going to enumerate the set of parsers, and ways of
combining them, by defining a type for representing an abstract syntax tree
(AST). To this purpose, we'll use a
[GADT](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type), that
also expresses that different parsers are indexed by different types. The
initial constructors are split into two sets -- primitive ones (the
leaf nodes of the tree), and combinators for composing parsers:

```ocaml
type 'a t =
  (* Primitive parsers *)
  | Fail      : string            -> 'a t
  | Empty     :                       unit t
  | Return    : 'a                -> 'a t
  | Symbol    : char              -> char t
  (* Composition - applicative *)
  | Map       : ('a -> 'b) * 'a t -> 'b t
  | Product   : 'a t * 'b t       -> ('a * 'b) t
  (* Composition - alternative *)
  | Either    : 'a t * 'a t       -> 'a t
```

The primitive parsers are:

  - `Fail msg` - a parser that always fails.
  - `Empty` - a parser that succeeds when given empty input.
  - `Return x` - a parser that does not consume any input and always returns `x`.
  - `Symbol c` - a parser that matches input when the first character is `c`.

The applicative interface is what provides sequential composition, as in:
first parse `x` with parser `p1`, then parse `y` with parser `p2` and combine
their results.

The `Either` constructor makes it possible to provide alternative execution
paths, i.e. parsers that succeed on different types of input.

As we don't want to give users direct access to the type, we'll mechanically
add some smart constructors:

```ocaml
let empty = Empty

let fail m = Fail m

let return x = Return x

let symbol c = Symbol c

let map f x = Map (f, x)

let product p q = Product (p,q)

let either p q = Either (p,q)
```

Having an applicative interface means that it is also possible to leverage
the new syntax extension -- the `let+ .. and+` notation -- which I've described
[here](http://jobjo.github.io//2019/04/24/ocaml-has-some-new-shiny-syntax.html).
Assuming OCaml 4.08 or the dune
[future_syntax stanza](https://discuss.ocaml.org/t/let-syntax-backported-to-ocaml-4-02/3447)
, we can add a syntax module, like so:

```ocaml
module Syntax = struct
  let (let+) p f = map f p
  let (and+) p q = product p q
end
```

In addition to these, we'll include an `Ops` module with infix versions
and some derived combinator:

```ocaml
module Ops = struct
  open Syntax
  let ( <$> ) f p   = map f p
  let ( <|> ) p q   = either p q
  let ( <*> ) pf px = let+ f = pf and+ x = px in f x
  let ( *>  ) p q   = (fun _ x -> x) <$> p <*> q
  let ( <*  ) p q   = const <$> p <*> q
end
```

Before moving on, some of the code examples also assume a few general
utility functions:

```ocaml
(* Identity *)
val id : 'a -> 'a

(* Const *)
val const : 'a -> 'b -> 'a

(* Forward composition *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(* Converting a char list to a string *)
val string_of_list : char list -> string

(* And back again *)
val list_of_string : string -> char list
```

Their implementation, along with the complete code is available
[here]().

### Building simple parsers

How can we use the API to build actual parsers? Let's consider a few simple
examples. First, a parser that parses a specific string, i.e. a function:

```ocaml
val string : string -> string t
```

To implement the `string` parser, we can use the `symbol` primitive and fold over the
given string to combine the parsers using applicative (`let+ .. and+)` syntax:

```ocaml
let string s =
  let accum c p =
    (* Parse 'x' using the 'symbol c' parser *)
    let+ x  = symbol c
    (* Then parse 'xs' using the 'p' parser *)
    and+ xs = p in
    (* Combine 'x' and 'xs' in a string *)
    Printf.sprintf "%c%s" x xs
  in
  List.fold_right accum (string_to_list s) (return "")
```

Next, let's attempt a parser for parsing digits. A version that
detects a single digit may be defined as:

```ocaml
let digit =
  list_of_string "0123456789"
  |> List.map symbol
  |> List.fold_left either fail
```

Choosing between a list of parsers is a natural generalization of the binary `either`
combinator, and deserves its own version:

```ocaml
let choose xs = List.fold_left either fail
```

We may also generalize the `digit` definition to take a list of symbols as an argument:

```ocaml
let one_of cs = choose @@ List.map symbol cs
```

Now, `digit` is achieved by:

```ocaml
let digit = one_of "0123456789"
```

### Hitting the boundaries

Next, consider a parser that recognizes arbitrary integers? An integer
consists of at least one digit but we don't know exactly how many. How can we
define a parser that captures this semantics? As a first try:

```ocaml
let int =
  let rec digits () =
    let+ d  = digit
    and+ ds = either (digits ()) (return []) in
    d :: ds
  in
  map (string_of_list >> int_of_string) @@ digits ()
```

Can you spot the problem with the above definition? If not, just try running
it and you'll find it throwing a *stack-overflow* exception. The problem is
that the recursive call is never conditional on any base case and is always
eagerly evaluated. We need a way to describe parsers that may consume
arbitrarily large inputs without constructing infinite parsing expressions!
To generalize from the `int` example, we're aiming for a combinator with the
following signature:


```ocaml
val many : 'a t -> ('a t) list
```


One solution would be to introduce a constructor, say `Delay`,
for representing lazy parsers:


```ocaml
type 'a t =
  ...
  | Delay : (unit -> 'a t) -> 'a t

let delay f = Delay f
```

That is a parser with delayed construction. We may use `delay` to define
`many`, as in:

```ocaml
let rec many p =
  let many_one =
    let+ x  = p
    and+ xs = delay @@ fun _ -> many p) in
    x :: xs
  in
  either many_one (return [])
```

Now, each step of the recursion is evaluated on demand rather than upfront.
This solution would work well if it weren't for the more ambitious set of constraints
having to do with pretty printing and symbol extraction. The problem is that
in order to extract all possible symbols of a delayed parser, we'd need to evaluate
it; this would unroll the infinite recursion expressed in the `many` definition, and
once again kill the stack.

### *Fixing* the parser definition

Is there any fix for the problem of simultaneously having a finite
traversable representation, and providing sufficient expressive power for
describing infinite parsers? The clue is in the question. A way of expressing
recursive structures without recursion is exactly what is offered by the [fixed-point
combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator).

Let's extend the parser type with a fixed-point constructor (and also get rid of `Delay`):

```ocaml
type 'a t =
  ...
  | Fix : ('a t -> 'a t) -> 'a t

let fix f = Fix f
```

We can then use `fix` as a remedy for the recursiveness of the definition of `many`,
from above:

```ocaml
let many p =
  fix @@ fun many ->
    let many_one =
      let+ x  = p
      and+ xs = many in
      x :: xs
    in
    either many_one (return [])
```

Note that there is *no* `rec` keyword in sight. The function `fix` is just a
handy tool for allowing us to mimic recursive functions. You may be left
wondering how exactly this solves the problem of symbol extraction, given
that we still end up with `Fix` nodes -- functions of type `('a t -> 'a t)`
-- that need to be evaluated. Hopefully, the next sections on interpreting
parsers will bring some clarity on that matter.

Coming back to the example of integer-parsing, here's how `int` may be defined
in terms of `many`:

```ocaml
let int =
  let+ d  = digit
  and+ ds = many digit in
  int_of_string @@ string_of_list (d :: ds)
```

Again, we may extract the common pattern of parsing one or more times using the same
parser (one or more digits in the example above) by adding a new combinator, as in:

```ocaml
let many_one p =
  let+ x = p
  and+ xs = many p
  x :: xs
```

Given a parser `p`, the parser `many p` succeeds if it can apply `p` *at least* one
time on its input.

As a side note, the code generously makes use of the new applicative syntax to
demonstrate how it may be used to write declarative code that also avoids
infix operators. As an alternative, we can define the same functions
without relying on the syntax extension; for instance:

```ocaml
(* Requires Ops module to be open *)

let many p = fix @@ fun many ->
  either (List.cons <$> p <*> many) (return [])

let many_one p = List.cons <$> p <*> many p

let int =
  (string_of_list >> int_of_string) <$> many_one digit
```

I'll leave it to the reader to conclude which style is preferable.

```ocaml
(* Requires Ops module to be open *)
let int =
  (string_of_list >> int_of_string string_of_list) <$> many_one digit
```

To wrap up with the initial set of parser examples, here is an implementation
of a *float parser* that also supports parsing values in scientific notation:

```ocaml
let float =
  (* Ex: 123. *)
  let p1 =
    let+ ds = many_one digit
    and+ d  = symbol '.' in
    ds @ [d]
  in
  (* Ex: 123.45 *)
  let p2 =
    let+ ds1 = p1
    and+ ds2 = many_one digit in
    ds1 @ ds2
  in
  (* Ex: 12.34e56 or 12e34 *)
  let p3 =
    let+ ds1 = p2 <|> many_one digit
    and+ e   = symbol 'e'
    and+ ds2 = many_one digit in
    ds1 @ [e] @ ds2
  in
  let fol cs = float_of_string @@ string_of_list cs in
  fol <$> choice [p1; p2; p3]
```

### Evaluating parsers

Up until this point, the focus has been on designing a sufficiently
expressive parser language and we've even implemented some simple examples, for
parsing decimal numbers. However, there is not a single line of code
concerning how to run a parser on actual input. We also have not
made any decisions about the exact semantics of what that would look like.
For instance, should running a parser return a list of all possible results
or only the first valid one it finds? Do we prefer the parser to backtrack
one or more steps in case it gets stuck?

The lack of such design decisions is a feature, not a bug! Carefully
designing a *frontend* for constructing parsers, allows us to provide multiple
*backends* for running them. At first we'll look at a simple non-backtracking
evaluator.

For executing a parser we need to turn it into a function. For a simplistic
version that does not deal with error reporting, we can use the same type  as
described above -- a function of type: `char list -> ('a * char list) option`:


```ocaml
val eval : 'a t -> (char list -> ('a * char list) option)
```

I've included a redundant set of parentheses to emphasize the fact that
`eval` take a parser and returns a function.

Given the recursive definition of the parser type, we naturally need a recursive
implementation.

Because of the GADT structure, each recursive call may invoke the function
with a different type of parser; for that reason, we need to use the [syntax
for polymorphic locally abstract
types](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#s%3Agadts).
Following is a complete implementation. It assumes a module `Option.Syntax`
for applicative and monadic combinators for option types, as described
[here](http://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html):


```ocaml
let rec eval :  type a. a t -> char list -> (a * char list) option = fun p ->
  match p with
  | Fail _ ->
    const None
  | Empty ->
    fun cs ->
      ( match cs with
        | []  -> Some ((), [])
        | _   -> None
      )
  | Return x  ->
    fun cs -> Some (x, cs)
  | Symbol s ->
    fun cs ->
      ( match cs with
        | c :: cs when c = s  -> Some (s, cs)
        | _                   -> None
      )
  | Map (f, p) ->
    let ep = eval p in
    fun cs ->
      Option.Syntax(
        let+ (x, cs) = ep cs in
        (f x, cs)
      )
  | Product (p, q) ->
    let ep = eval p in
    let eq = eval q in
    fun cs ->
      Option.Syntax(
        let* (x, cs) = ep cs in
        let* (y, cs) = eq cs in
        Some ((x,y), cs)
      )
  | Either (p, q) ->
    let ep = eval p in
    let eq = eval q in
    fun cs ->
      ( match ep cs with
        | Some r -> Some r
        | None   -> eq cs
      )
  | Fix f ->
    fun cs -> eval (f (Fix f)) cs
```

Each branch of the pattern match returns a function that accepts
a `char list` and produces an optional result.

For convenience we can expose it as a function that transforms a parser
specification into a function consuming strings for actually doing the parsing:

```ocaml
val eval' : 'a t -> string -> ('a * char list) option
```

Implemented as:

```ocaml
let eval' p = list_of_string >> eval p
```

Following are a few examples, testing the evaluator on the `float` parser,
defined above:

```ocaml
# eval' float "12.34";;
- : (float * char list) option = Some (12.34, [])

# eval' float "1.2e3";;
- : (float * char list) option = Some (1200., [])

# eval' float "a1.23";;
- : (float * char list) option = None
```

The implementation of `eval` is not backtracking and only returns one possible
match of the parser for a given input. An alternative implementation would
return a list of results, as in:

```ocaml
val eval : 'a t -> 'a char list -> 'a list
```

This implementation is left as an exercise.

### Extracting symbols

Symbol extraction is another example of an interpreter for parsers -- a
function that identifies the set of all possible inputs to a parser by
traversing its tree:

```ocaml
val symbols : 'a parser -> char list
```
As with `eval`, it is defined as a recursive function that handles all parser
constructors:


```ocaml
module CS = Set.Make (Char)

let symbols p =
  let rec aux : type a. a t -> CS.t = function
    | Fail _        -> CS.empty
    | Empty         -> CS.empty
    | Return _      -> CS.empty
    | Symbol c      -> CS.singleton c
    | Map (_, p)    -> aux p
    | Product (p,q) -> CS.union (aux p) (aux q)
    | Either (p,q)  -> CS.union (aux p) (aux q)
    | Fix f         -> aux @@ f @@ Fix (fun _ -> Fail "")
  in
  aux p |> CS.to_seq |> List.of_seq
```

Most cases are trivial -- for instance, `Symbol c` returns a singleton set
with `c`, and `Either` returns the union of the symbols from each branch.
The `Fix f` is perhaps the most interesting case. The payload of `Fix` is a
function of type: `'a t -> 'a t`, so we just just need to pass it some parser
that does not introduce any additional symbol, hence the `Fail`.

We can try it out on the `float` parser:

```ocaml
# symbols float;;
- : CS.elt list =
['.'; '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'e']
```

### Showing parsers

As a final example of a parser interpreter, we'll write a function for converting
parsers into strings:

```ocaml
val show : 'a t -> string
```

Once again, it follows a familiar pattern where we map each constructor:

```ocaml
let rec show : type a. a t -> string = function
  | Fail msg ->
    msg
  | Empty ->
    "empty"
  | Symbol c ->
    Printf.sprintf "symbol '%c'" c
  | Return x ->
    "return ?"
  | Map (f, p) ->
    Printf.sprintf "map ? (%s)" (show p)
  | Product (p,q) ->
    Printf.sprintf "product (%s) (%s)" (show p) (show q)
  | Either (p,q) ->
    Printf.sprintf "either (%s) (%s)" (show p) (show q)
  | Fix f ->
    Printf.sprintf "let rec f () = %s) in f ()" @@ show (f (Fail "f ()"))
```

Note that we're not able to fully display all constructions. The payload
of `Map` consists of a function that we just don't know how to print. We also
lack a printer for the argument to `return`. For fully transparent parsers
we'd have to impose some additional structure -- such as pairing arguments with
printers -- but for now we just insert some question marks to indicate the
opaque arguments. Note also how we recover the recursive structure of `Fix f`
by printing it as `let rec .. `. As an example, consider printing a parser
obtained by the `many` combinator:

```ocaml
# let p = many (symbol 'a');;
val p : char list P.t = Fix <fun>
# print_endline @@ show p;;
let rec f () = either (map ? (product (map ? (symbol 'a')) (f ()))) (return ?)) in f ()
```

To see how this makes sense, here's a version where the question marks
have been filled in:


```ocaml
let rec f () =
  either
    (map
      List.cons
      (product
        (map id (symbol 'a'))
        (f ())
      )
    )
    (return [])
in
f ()
```

This very much resembles the initial recursive version of `many` that we
started off with, before introducing the fixed-point combinator.

### Optimizing the evaluator

Looking at the implementation of `eval`, you may have noticed that in every
case -- except for `Fix f` -- there are no recursive calls in the body of the
returned functions. For instance, in the `Product (p,q)` case, both `p` and
`q` are evaluated upfront, before returning a function that consumes actual
input. Thus, for parsers not using `fix` -- all traces of the parser AST are
eliminated by `eval`, and we pay no additional run-time cost during actual
parsing.

Is it possible to amend the case for `Fix f` to achieve evaluation upfront?
A first attempt would be to simply factor out the recursive call:

```ocaml
    | Fix f ->
      let g = eval @@ f (Fix f) in
      fun cs -> g cs
```

One realizes quickly, however, that this wont' quite cut it as evaluation may
get stuck on ever-expanding `f (Fix f)` expressions. Remember that
`fix` was introduced to encode recursive definitions required for
the `many` combinator. The strategy is to transform it back to a
recursive definition at evaluation level, similar to what we did for printing
in the implementation of `show`, above.

In other words, we need to go from:

```ocaml
Fix f
```

To, an expression of the form:

```ocaml
let rec aux () =
  ...
  aux ()
in
aux ()
```

Where the body of `aux` is given by applying the function `f` to some
other expression that may call back to itself. The problem, however, is
that the argument to `f` must itself be parser expression. We can work around
this by introducing another node for keeping a *raw* evaluator:


```ocaml
  type 'a t =
    ...
    | Raw : (char list -> ('a * char list) option) -> 'a t
```

Exposing such a constructor would of course defeat the purpose of having
a fixed-point combinator in the first place, as we'd have to give up symbol
extraction and printing anyway. However, we'll only use it as an internal
*helper* node. If we exclusively export the smart constructors, and make the type
`'a t'`, either abstract or *private*, users would never be able to construct
*raw* nodes. We can thus safely ignore such cases for any other evaluator.

Coming back to the implementation of `eval`, here's how the recursive transformation
is achieved:

```ocaml
let rec eval :  type a. a t -> char list -> (a * char list) option = fun p ->
  ...
  | Fix f ->
    let rec k = lazy (eval @@ f (Raw (fun cs -> Lazy.force k cs))) in
    Lazy.force k
  | Raw f ->
    f
```

The recursive definition of `k`, is calling `eval` lazily. However, since we
immediately force it, we only ever evaluate it once! The expression returned
by applying `f` may contain a sub-node `Raw g`, where `g` refers back to `k`.
An alternative option would be to use ref-cells instead of the *lazy* constructs.

### A few more tweaks

Before looking at another example of how to use the parsing library, there
are a few utility functions that will come in handy:

```ocaml
val one_of  : string -> char t
val between :'a t -> 'b t -> 'c t -> 'c t
val forget  :'a t -> a t
```

Here:

  - The parser `one_of s` succeeds on any symbol from the given string `s`.
  - `between p1 p2 p` is a parser that first parser `p1`, followed by `p`, followed
    by `p2` and only returns the result of `p`.
  - The parser `forget p`, runs `p` and returns its result without consuming any input

The functions `one_of` and `between` may be written in terms of the
existing combinators. As for `forget`, it's useful for determining that a parser is
followed by another parser by *looking ahead*, and requires a new node
in the parser AST:

```ocaml
type 'a t =
  ...
  | Forget    : 'a t -> 'a t
```

Finally, it's a bit silly to only expose a primitive `Token` constructor
for parsing a single character, when so many parsers are based on parsing
specific strings -- such as keywords in some grammar.  As a pure optimization
strategy, we can replace `Token` with a more generalized version that accepts
any character from a given set:

```ocaml
module CS = Set.Make (Char)

type 'a t =
  ...
  | One_of    : CS.t -> char t
```

The complementing code changes are straight forward.


### An example -- parsing *s-expressions*

To give a more elaborate example of writing a parser using this library, we'll
consider parsing *s-expressions*.

An s-expression may be represented in OCaml by a data type like:

```ocaml
type atom =
  | Int of int
  | Float of float
  | String of string
  | Symbol of string

type sexp =
  | Atom of atom
  | List of sexp list
```

The goal is to define a parser of type `sexp t`, that can parse strings into
`sexp` values. Here's an example:

```
(var "x" ((times (plus 1 2) (val "y"))))
```

This string would be parsed into the following OCaml value:

```ocaml
List
  [Atom (Symbol "var"); Atom (String "x");
  List
    [List
      [Atom (Symbol "times");
      List
        [Atom (Symbol "plus"); Atom (Int 1);
        Atom (Int 2)];
      List
        [Atom (Symbol "val"); Atom (String "y")]]]]
```

The parser should differentiate between different types of input, such
as *symbols*, *integers*, *floats* and (quoted) strings, which may also contain
spaces and parentheses.

We'll start with a few helpers for parsing different types of strings:

```ocaml
let space         = one_of "\n\t\r "
let left_paren    = symbol '('
let right_paren   = symbol ')'

let regular_char =
  let special_char  = list_of_string "()\n\r\t\" " in
  let is_regular c = not @@ List.mem c special_char in
  List.init 256 Char.chr |> List.filter is_regular |> string_of_list |> one_of

let regular_string = string_of_list <$> many_one regular_char

let quoted_string =
  let quote   = symbol '\"' in
  let string  = many (regular_char <|> space <|> left_paren <|> right_paren) in
  string_of_list <$> between quote quote string
```

It's now fairly straight forward to combine the functions for parsing `atom`s:

```ocaml
let atom =
  let end_num =
    forget (empty <|> map ignore space <|> map ignore right_paren)
  in
  choice
    [ map (fun i -> Int i )   int <* end_num
    ; map (fun f -> Float f)  float <* end_num
    ; map (fun s -> String s) quoted_string
    ; map (fun s -> Symbol s) regular_string
    ]
```

Since the `sexp` type is recursive, we'll again turn to `fix` to express
that as a parser:

```ocaml
let sexpr =
  fix @@ fun sexpr ->
    let expr =
      either
        ((fun es -> List es) <$> between left_paren right_paren (many sexpr))
        ((fun a  -> Atom a ) <$> atom)
    in
    between (many space) (many space) expr
```

### Conclusion

This post covered quite a few different concepts, including:

- Parser combinators.
- Difference between *monadic* and *applicative* parser APIs.
- Applicative functor composition and complementary `let+ .. and+ ..` syntax.
- GADTs for representing abstract syntax trees.
- Fixed-point combinators for encoding recursive expressions.

It serves to exemplify a more general pattern -- related to [initial
algebras](https://en.wikipedia.org/wiki/Initial_algebra) -- where the approach
is to identify a data type, for a particular domain, while making as few
commitments as possible. Whenever we define a value in the algebra -- in
our case, the parser algebra -- we don't loose any information that went into
constructing it. We can thus map it to other more refined representations.
For parsers, this meant providing multiple backends in the form of
*evaluator* and *printer* functions.

You'll find the full code, including the examples,
[here](https://gist.github.com/jobjo/13376aaea1151100dd7915dedb35d9d7).