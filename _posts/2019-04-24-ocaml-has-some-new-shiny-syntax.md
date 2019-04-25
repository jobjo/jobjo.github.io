---
layout: post
title: OCaml has some new shiny syntax
---

The next major release of the OCaml compiler, version 4.08, will be equipped
with a [new syntax extension](https://github.com/ocaml/ocaml/pull/1947) for
*monadic* and *applicative* composition. Practically it means that it will be
a bit more convenient to work with APIs structured around these
patterns. The design draws inspiration from
[ppx_let](https://github.com/janestreet/ppx_let) but offers lighter syntax,
and removes the need of running the code through a [ppx preprocessor](https://victor.darvariu.me/jekyll/update/2018/06/19/ppx-tutorial.html).

Compared to similar extensions for languages like Haskell, F# and Scala, it's
interesting to note that the OCaml version is not only targeting monads but
also supports a version for [applicative
functors](http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf).

This post contains some concrete examples of what the new syntax looks like
and how to enable it.

As of writing, version 4.08 has not been officially released so in order to
follow along you need to update your opam and switch to the beta release:

```fsharp
opam switch ocaml-variants.4.08.0+beta1
```

Alternatively, you can also use the latest version of `dune` for building, which
[has a backport of the syntax extension](https://discuss.ocaml.org/t/let-syntax-backported-to-ocaml-4-02/3447).

## An example - working with options

The examples below are about composing functions returning
optional results using the monad and applicative functor combinators for
options.

To have something concrete to work with, assume the following API:

```ocaml
val safe_head : 'a seq -> 'a option
val safe_tail : 'a seq -> 'a seq option
val safe_div : float -> float -> float option
```

### Monad syntax for options

The monadic composition combinators for the `option` type may be defined
as a function `bind` with the signature:

```ocaml
val bind : 'a option -> ('a -> 'b option) -> 'b option
```

Implemented as:

```ocaml
let bind o f =
  match o with
  | Some x  -> f x
  | None    -> None
```

The convention is to also provide an infix version, as in:

```ocaml
let ( >>= ) o f = bind o f
```

It's effectively used for composing sequences of functions yielding optional
results.

As a silly example, consider writing a function that given a `float seq`,
returns the value of the first two elements divided, if the sequence contains
at least two elements, and the division is successful. Using the API from
above and the monadic bind operator, it may be defined as:

```ocaml
let div_first_two xs =
  safe_head xs >>= fun x ->
  safe_tail xs >>= fun ys ->
  safe_head ys >>= fun y ->
  safe_div x y
```

We can write this differently using the syntax extension for monads, all that
is needed is another alias to `bind`:

```ocaml
let (let*) x f = bind x f
```

That's it, the same program can now be expressed as:

```ocaml
let div_first_two xs =
  let* x  = safe_head xs in
  let* ys = safe_tail xs in
  let* y  = safe_head ys in
  safe_div x y
```

In general, the expression:


```ocaml
let* x = e1 in e2 x
```
desugars to the equivalent of:

```ocaml
e1 >>= fun x -> e2 x
```

### Applicative syntax for options

Monads are great for composing dependent computations but not all
compositions are dependent. For the cases where monads are either an
overkill or just not feasible, applicative functors provide an alternative. You
can find some more information about this pattern in the context of OCaml,
[here](http://blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html);
They are often described in terms of two functions `pure` and
`apply`, with the signatures:


```ocaml
val pure : 'a -> 'a t
val apply  : ('a -> 'b) t -> 'a t -> 'b t
```

The infix version of `apply` is usually called `(<*>)`, i.e.:


```ocaml
let ( <*> ) fa xa = apply fa xa
```

How exactly do they supplement monads? By looking at the signature of
`apply`, it is clear that in the expression, `f <*> x`, both `f` and `x` are
applicative values that exist before the evaluation of `apply` is performed.

In contrast, in the monadic composition expression, `x >>= f`, the monad
value produced by `f` is not known until it's actually applied a value extracted
from `x`.

We, therefore, say that applicatives provide *static* composition whereas
monads also support *dynamic* composition. In short, this makes monads more
powerful but less optimization friendly.

Below are the traditional applicative combinators defined for the `option` type:

```ocaml
let pure x = Some x

let apply fo xo
  match fo, xo with
  | Some f, Some x  -> Some (f x)
  | _               -> None

let (<*>) fo xo = apply fo xo

```

And, here's an example of how they're used in defining a function that given
three `float seq` values, adds their heads together, in case they're all non-empty:

```ocaml
let add_heads xs yz zs =
  pure (fun x y z -> x +. y +. z)
  <*> safe_head xs
  <*> safe_head ys
  <*> safe_head zs
```

This function could of course also be written in a monadic style but what's
nice about the applicative version is that it's apparent from the definition that
there are no dependencies between the three calls to `safe_head`.

If we were operating in some other applicative context, say `Async.t`, we could in
fact run the extractions in parallel.

As for the OCaml syntax extension for applicatives, it's actually not based on
`apply` and `pure`, but a pair of alternative combinators, often called `map`
and `product`:

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
val product : 'a t -> 'b t -> 'a * 'b t
```

To show that `apply` and `product` are equivalent, here's how you define
the infix version of `apply` in terms of `map` and `product`:

```ocaml
let ( <*> ) fa xa = map (fun (f, x) -> f x) @@ product fa xa
```

Going the other way around, one can also express `product` using `pure` and `apply`:

```ocaml
let product xa ya = pure (fun x y -> (x, y) <*> xa <*> ya
```

The implementations of `map` and `product` for the `option` type are straight forward:

```ocaml
let map f = function
  | None    -> None
  | Some x  -> Some (f x)

let product o1 o2 =
  match o1, o2 with
  | Some x, Some y  -> Some (x,y)
  | _               -> None
```

Now, to enable the special syntax we just need to define `(let+)` as `map` with arguments
in reversed order and `(and+)` as an alias for `product`:

```ocaml
let (let+) x f    = map f x
let (and+) o1 o2  = product o1 o2
```

Finally, using the syntax extension, we can rewrite the example above, like this:

```ocaml
let add_heads xs yz zs =
  let+ x = safe_head xs
  and+ y = safe_head ys
  and+ z = safe_head zs in
  x +. y +. z
```

Again, the syntax stresses how the three `safe_head` computations are
independent. In fact, the compiler prevents us from introducing any
dependencies (just like with regular) `let .. and` syntax).

In general, an expression, `let+ x = e1 and+ y = e2 in e3 x y`, gets desugared to:

```ocaml
map (fun (x,y) -> e3 x y) (product e1 e2)
```

Or the equivalent expression, written in terms of `apply` and `pure`:

```ocaml
e3 <$> e1 <*> e2
```

Here, `(<$>)` is the infix version of `map`.

### Wrapping up

To conclude the examples for `option`s, here's a version where the operations
are wrapped in a module `Option` with the syntax extensions contained in a
sub module. This allows users of the library to opt in for the new
syntax when needed (by opening `Option.Syntax`):

```ocaml
module Option = struct

  let map f = function
    | None    -> None
    | Some x  -> Some (f x)

  let bind o f =
    match o with
    | Some x  -> f x
    | None    -> None

  let product o1 o2 =
    match o1, o2 with
    | Some x, Some y  -> Some (x,y)
    | _               -> None

  module Syntax = struct
    let (let+) x f    = map f x
    let (and+) o1 o2  = product o1 o2
    let (let*) x f    = bind x f
  end

end
```

So, whenever you wish to extend your data type, `t`, with
special syntax, implement a module like `Syntax`, with the following
signature:


```ocaml
module Syntax : sig

  (* Monad *)
  val ( let* ) : 'a t -> ('a -> 'b t ) -> 'b t

  (* Applicatives *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t-> ('a * 'b) t

end
```

If you're only able to support the applicative subset, simply skip `( let* )`.

## Final notes

As with any language extension, there is a tradeoff between the utility introduced
and the additional complexity or cognitive load required in order to benefit from
it. In this case I think it's worth the price. In particular, it's likely going to
nudge developers toward the time-tested patterns of monads and applicative
functors. Having more libraries with common looking APIs actually works in the opposite
direction, reducing the amount of cognitive load required.

