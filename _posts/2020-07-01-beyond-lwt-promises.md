---
layout: post
title: Beyond Lwt promises
---

At work, we are heavy users of the OCaml [Lwt](https://github.com/ocsigen/lwt)
library for promised based concurrent programming. Lwt is popular, actively
developed, has exceptional performance, and can run on different platforms!

In this post, however, I would like to discuss some of its limitations when
it comes to asynchronous programming. The typical use case would be writing
a service that aggregates a bunch of data from other services and/or
databases.

The purpose is not to criticize the design of Lwt, it is merely to point out
that there are alternative abstractions that may work better for some scenarios.
Although I'll be focusing on Lwt, this is really part of a broader discussion
contrasting eagerly evaluated promises with lazy futures.

For an introduction to the Lwt library, see its [excellent
documentation](https://ocsigen.org/lwt/5.2.0/manual/manual) or check out
[these blog posts](https://raphael-proust.github.io/code/lwt-part-1.html).

## What is run and when?

The first challenge when looking at a program written in Lwt is figuring
out what things are executed and why. Consider this example:

```ocaml
open Lwt.Syntax

let program1 =
  let* () = Lwt_io.printl "Kicking off 1" in
  let* () = Lwt_io.printl "Running 1" in
  let* () = Lwt_unix.sleep 1. in
  Lwt.return 1

let program2 =
  let* () = Lwt_io.printl "Kicking off 2" in
  let* () = Lwt_unix.sleep 1. in
  let* () = Lwt_io.printl "Running 2" in
  Lwt.return 2
```

Here we have two top-level definitions, `program1` and `program2` but no
`Lwt_main.run` instruction. What does the program output? As Lwt promises are
eagerly evaluated, both `program1` and `program2` start executing but halts on
their first sleep command. Since there is no scheduler to resume their
operations they are never completed. The output is:

```
Kicking off 1
Running 1
Kicking off 2
```

What if we *run* `program1`?

```ocaml
let _ = Lwt_main.run program1
```

As expected, `program1` now completes but perhaps surprisingly, the remaining
actions of `program2` are also picked up, why the output is:

```
Kicking off 1
Running 1
Kicking off 2
Running 2
```

I should say that this is made transparent by the documentation and perfectly
in line with the design. There is a global context that knows about all Lwt
promises and executes them explicitly or implicitly.

## Implicit parallelism

When writing a typical orchestration program that calls various services we
need to have control over which operations to run in sequence and what
should be performed concurrently. For some programs written in Lwt, this is not
always obvious. First, looking at the following snippet:

```ocaml
let call_robert () =
  let* () = Lwt_io.printl "Calling Robert" in
  let* () = Lwt_unix.sleep 1. in
  let* () = Lwt_io.printl "Robert picked up." in
  Lwt.return "Hello"

let call_maria () =
  let* () = Lwt_io.printl "Calling Maria" in
  let* () = Lwt_unix.sleep 3. in
  let* () = Lwt_io.printl "Maria picked up" in
  Lwt.return "Hola"

let program =
  let* _ = call_robert () in
  let* _ = call_maria ()  in
  Lwt.return ()

let _ = Lwt_main.run program
```

By reading the code, it should be clear that `call_robert ()` and `call_maria ()`
are executed in sequence. The output is:

```
Calling Robert
Robert picked up.
Calling Maria
Maria picked up
```

What happens if we instead rewrite the program to the version below?

```ocaml
let robert = call_robert ()

let maria = call_maria ()

let program =
  let* _  = robert in
  let* _ = maria in
  Lwt.return ()
```

From the definition of `program` it may still seem like two sequential
operations. However, since both promises `robert` and `maria` are defined
outside the scope of the monadic composition, they start executing in the
background concurrently; Confirmed by the output:

```
Calling Maria
Calling Robert
Robert picked up.
Maria picked up
```

## Referential transparency

The underlying principle that makes some programs written in Lwt difficult to
reason about is that they are not [referentially
transparent](https://en.wikipedia.org/wiki/Referential_transparency). It
means that you cannot safely refactor code by lifting out some common
sub-expressions and giving them names. Let's look at another concrete
example:

```ocaml
let increment_count =
  let n = ref 0 in
  fun () ->
    incr n;
    let* () = Lwt_unix.sleep 1. in
    Lwt.return (!n)

let program () =
  let* c1 = increment_count () in
  let* () = Lwt_io.printf "Count is %d\n" c1 in
  let* c2 = increment_count () in
  let* () = Lwt_io.printf "Count is %d\n" c2 in
  Lwt.return ()

let _ = Lwt_main.run @@ program ()
```

Here the important aspect of `increment_count` is that it performs some
observable side-effect -- think calling a database for a record update. As
expected, when run, this program yields:

```
Count 1
Count 2
```

Since there are two identical calls to `increment_counter`, and because in
functional programming we're used to relying on referential transparency, it
is tempting to refactor the code, as in:

```ocaml
let program () =
  let incr = increment_count () in
  let* c1 = incr in
  let* () = Lwt_io.printf "Count %d\n" c1 in
  let* c2 = incr in
  let* () = Lwt_io.printf "Count %d\n" c2 in
  Lwt.return ()

let _ = Lwt_main.run @@ program ()
```

That is not a safe refactoring as it changes the behavior of the program -- now we only
increment the count once and the output is:

```
Count 1
Count 1
```

## Impossible combinators

Because Lwt promises are evaluated eagerly there are useful functions that
simply cannot be given meaningful implementations. For instance, imagine a
combinator that would attach timing information to the execution of a given
promise:

```ocaml
val timed : 'a Lwt.t -> ('a * float) Lwt.t
````

Or, a function for batching parallel requests:

```ocaml
val parallel : batch_size:int -> ('a Lwt.t) list -> ('a list) Lwt.t
```

And a corresponding version for sequencing a bunch of promises:

```ocaml
val sequence : ('a Lwt.t) list -> ('a list) Lwt.t
```

The reason they are not feasible is that any promise passed to one of these
functions is already running in the wild and any attempt at scheduling it or
timing it is futile. A workaround is to replace promises of type `'a Lwt.t`
with values of type `unit -> 'a Lwt.t` but it does make the API less elegant
and composable.

## Alternative designs

If you agree with my reasoning above and think it's worth considering
a different semantics, it does not necessarily require throwing out the
baby out with the bathwater. We may still be able to rely on the Lwt
machinery for scheduling and maintaining compatibility with existing Lwt
based libraries. What I propose is introducing an abstraction based on
unevaluated *actions* rather then promises.

The mental model for a promise is that of a mutable reference cell that may
or may not contain a value, and if not, may eventually be filled with a value
or an exception.

An *action*, on the other hand, would represent a *computation* that when run may
produces a value. Promises are inherently imperative while actions are
functional (referentially transparent).

As a simple starting point, we could define an (an abstract) action type as:

```ocaml
type 'a t = unit -> 'a Lwt.t
```

This allows for the same monadic interface we're used to from Lwt but with
stronger guarantees that code that looks like its executed sequentially,
really is. In our example from above:

```ocaml
let robert = call_robert ()

let maria = call_maria ()

let program =
  let* _  = robert in
  let* _ = maria in
  Lwt.return ()
```

It no longer matters that `robert` and `maria` are bound in an outer scope. They
represent instructions for *how* to calculate some values, not the values themselves.
Nothing is executed until explicitly requested, which would be achieved by
invoking a corresponding run function:

```ocaml
let run action = Lwt_main.run (action ())
```

To express parallel operations we can simply define the `let* .. and*` syntax
similar to what exists in Lwt, and use it as in:

```ocaml
let program =
  let* _ = robert
  and* _ = maria in
  Lwt.return ()
```

Or, we could expose some parallel combinator. The point is that we now require the
parallel operations to be declared explicitly!

Thanks to referential transparency, we'd also recover the missing *count*:

```ocaml
let program =
  let incr = increment_count () in
  let* c1 = incr in
  let* () = Lwt_io.printf "Count %d\n" c1 in
  let* c2 = incr in
  let* () = Lwt_io.printf "Count %d\n" c2 in
  Lwt.return ()
```

Additionally, we are allowed to implement the impossible combinators from above:

```ocaml
val timed : 'a t -> ('a * float) t

val parallel : batch_size:int -> ('a t) list -> ('a list) t

val sequence : ('a t) list -> ('a list) t
```

This is by no means a novel idea and the lazy semantics of an action based
API is similar to what's provided by [F#'s
Async](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/asynchronous-and-concurrent-programming/async),
the [async library in Haskell](https://hackage.haskell.org/package/async) or
[Async/await in Rust](https://rust-lang.github.io/async-book/) among many
others.

Finally, if we feel like it, we could generalize the type further and also introduce:

- Error handling (similar in spirit to `Lwt_result`)
- An environment for passing down configuration options

Something along the lines of:

```ocaml
type ('r, 'a , 'e) t = 'r -> ('a, 'e) result Lwt.t
```

This would land us closer to the design of the [ZIO](https://zio.dev/) library in Scala.
