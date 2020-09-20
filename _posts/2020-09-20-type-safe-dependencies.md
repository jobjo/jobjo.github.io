---
layout: post
title: Type-safe and composable dependencies
---

Applications that need to communicate with the outside world inadvertently end up
accumulating a range of dependencies – things like database
connection-strings, logging facilities, or configuration options.

Running an application in a specific setting means instantiating a particular
set of configurations. For example, for testing purposes, we may want
to provide mock implementations of some functionality in order to achieve
deterministic results.

What’s a good strategy for factoring out such dependencies in OCaml? In this post I’ll
propose an approach that is:

- Type-safe
- Composable
- Does not require the whole code to be functorized

Before discussing the proposal, other options include using module-functors,
global mutable state or pass around all configurations explicitly.
I won’t discuss these in detail but they all come with trade-offs. Explicitly
passing around configuration objects adds verbosity, especially to
intermediate functions. Functorizing the code requires quite a bit of
heavy-lifting and often leads to some non-trivial design decisions regarding
the module hierarchy. Using global state has its own obvious drawbacks.

Here's a [discussion
thread](https://discuss.ocaml.org/t/dependency-injection-alternatives-for-web-development/6385/2)
which outlines a few more concrete options.

My proposed API is similar in sprit to effect systems but more limited in
scope and may be implemented in vanilla OCaml, rather than the upcoming
multi-core/effects version.

This strategy, however, does resort to using monads for providing the glue
code that pieces together different parts of resource-dependent computations.
Chances are that you're already working in some monad context, be it
`Result.t` or `Lwt.t` and the code may be adapted to extend these
as well. For simplicity, I'm not considering error handling or async actions
(ala Lwt) in this post, however.

The solution I'll sketch out is a tweaked version of the simple reader-monad
so it's worth taking a look at why exactly the standard version doesn't cut it.

## Limitations of the reader-monad

A simple reader-monad is just a function from some input type to some output
type, and may be defined as:

```ocaml
type (+'a, -'r) t = 'r -> 'a
```

A basic monadic API can be provided:

```ocaml
type (+'a, -'r) t

val return : 'a -> ('a, 'r) t

val map : ('a -> 'b) -> ('a, 'r) t -> ('b, 'r) t

val map_env : ('s -> 'r) -> ('a, 'r) t -> ('a, 's) t

val ( let* ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t

val ask : ('r, 'r) t

val run : 'r -> ('a, 'r) t -> 'a
```

You may think of a value of type `(a, r) t` as a computation that when
run needs to be supplied with a value of type `r`.

The `ask` function is used to fetch the value from the environment.

Note that the type is covariant in `a` and contravariant in `r`, hence the
the signatures or `map` and `map_env`. The function `map_env` is required in
order to mix computations that depend on different types of environments.

Here's an implementation of the signature:

```ocaml
type (+'a, -'r) t = 'r -> 'a
let return x _ = x
let map_env f m r = m (f r)
let map f m = fun r -> f (m r)
let ( let* ) m f = fun r -> f (m r) r
let ask r = r
let run e m = m e
```

Let's look at a schoolbook example of how to use it. Say that some components
of our application depends on a *user-id* value in the form of an integer. So
the `'r` part of the reader-monad in this example is `int`, and we can provide
access to the user-id via a function:

```ocaml
let get_user_id : (int, int) t = ask
```

And have code depend on it, as in:

```ocaml
(* val log : string -> (unit, int) t *)
let log s =
  let* user_id = get_user_id in
  Printf.printf "[User %d] %s" user_id s;
  return ()
```

The function `log` takes a string and prints it along with *user-id*. It can be
embedded in other computations constituting the top-level program, like so:

```ocaml
(* val program : (unit, int) t *)
let program =
  let* () = log "Warming up" in
  ...
  return ()
```

To run the `program` we need to supply the user-id:

```ocaml
let () = run 123 program
```

So far so good, but for real world scenarios one does not always control all
the resources upfront. That is, we need to combine computations that are
defined in different libraries and require their own sets of dependencies.

Imagine for example another module with definitions:

```ocaml
type log_mode = Local | Remote

let get_log_mode : (log_mode, log_mode) t = ask
```

This module provides an accessor to a *log-mode* value. If we were to make
use of this function for our custom `log`, which already depends on *user-id*,
we'd have to introduce a new type and use `map_env` to accommodate for both:

```ocaml
type user_id_and_log_mode = { user_id : int; log_mode : log_mode }

let log s =
  let* mode = map_env (fun { log_mode; _ } -> log_mode) get_log_mode in
  let* user_id = map_env (fun { user_id; _ } -> user_id) get_user_id in
  return
    ( match mode with
    | Local -> Printf.printf "[User %d] %s" user_id s
    | Remote -> failwith "Not implemented" )
```

Any time we use a set of dependencies we end up with new types. Note
that functions like `log` may be defined in libraries that are not of aware
of the complete environment required to run the top-level application.

All this mapping between environments adds up to chunks of boiler-plate
code and quickly breaks down as applications grow larger, and are made more
modular.

## An extensible reader-monad

Can we have a better reader-monad, one that can extend and combine different
types of resources from different contexts? OCaml provides a few features
that may come in handy: classes/objects, open types and [polymorphic
variants](https://dev.realworldocaml.org/variants.html#:~:text=In%20addition%20to%20the%20ordinary,supports%20so%2Dcalled%20polymorphic%20variants.&text=As%20you%20can%20see%2C%20polymorphic,about%20all%20of%20those%20tags.).

The encoding I suggest makes use of polymorphic variants and is inspired by
their applications for error handling, as described in [this
article](https://keleshev.com/composable-error-handling-in-ocaml).

Just like the vanilla reader-monad, a type `('a, 'r) t` is introduced and
represents a computation that depends on an
*environment* value of type `'r` and produces a value of type `'a`. The tweak
is to make the computation not directly dependent on `r` but on a
context/environment parameterized by `'r`. A module `Context` is therefore
provided with the following signature:

```ocaml
module Context : sig
  type value
  type 'a t
  val value : 'a -> 'a t -> value
end
```

It exposes a type, `value`, and a function for producing values by using the
context. Note however that it does not expose any means of creating new
contexts. That's a key when it comes to type-safety as will be seen below.
Before touching on the implementation, a similar monadic API to the reader
version is also provided:

```ocaml
type (+'a, 'r) t

type void

val return : 'a -> ('a, 'r) t

val map : ('a -> 'b) -> ('a, 'r) t -> ('b, 'r) t

val run : ('a, void) t -> 'a

val ( let* ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t

val provide : ('r -> Context.value) -> ('a, 'r) t -> ('a, 'v) t

val fetch : tag:('a Context.t -> 'r) -> ('a, 'r) t
```

It is only possible to *run* a computation whose context can be
unified with `void`. Since there are no ways of constructing `void` values
-- without raising an exception -- it's a way of expressing that the computation
must not depend on any environment resources.

To *fetch* a value from the context, all is that is needed specifying how to *tag* it.
Here's the corresponding `get_user_id` function from above:

```ocaml
let get_user_id () : (int, [> `User_id of int Context.t ]) t =
  fetch ~tag:(fun ctx -> `User_id ctx)
```

And an example of how to use it:

```ocaml
(* val log : string -> (unit, [> `User_id of int Context.t ]) t *)
let log s =
  let* user_id = get_user_id () in
  Printf.printf "[User %d] %s" user_id s;
  return ()
```

How do we run a top-level program that embeds one or several computations
depending on user-id? Consider:

```ocaml
let program =
  let* () = log "Warming up" in
  ...
  return ()
```

Passing it to the `run` function directly won't satisfy the compiler:

```ocaml
let _ = run program
```

This fails with:

```ocaml
Error: This expression has type
  (unit, [> `User_id of int Context.t ] as 'a) t
  but an expression was expected of type
  (unit, void) t
```

Before running it, we need to resolve all dependencies,
using the `provide` function:

```ocaml
val provide : ('r -> Context.value) -> ('a, 'r) t -> ('a, 'v) t
```

In this case user-id is the only required resource:

```ocaml
let _ =
  run @@
    provide
      (function `User_id ctx -> Context.value 123 ctx)
      program
```

The signature of `provide` takes a function for mapping a resource context to
a value. Note that the only way of constructing a value is to use the
embedded *int* sub-context and the `Context.value` function.

So far we've basically replicated the reader-monad. But importantly, we've
solved the problem of freely mixing resources. Here's the `get_log_mode`
example:


```ocaml
let get_log_mode () : (log_mode, [> `Log_mode of log_mode Context.t ]) t =
  fetch ~tag:(fun ctx -> `Log_mode ctx)
```

Along with a `log` function that combines the two resources *user-id* and
*log-mode*:

```ocaml
(*
val log :
  string ->
  (unit, [> `Log_mode of log_mode Context.t | `User_id of int Context.t ]) t
*)
let log s =
  let* mode = get_log_mode () in
  let* user_id = get_user_id () in
  return
    ( match mode with
    | Local -> Printf.printf "[User %d] %s" user_id s
    | Remote -> failwith "Not implemented" )
```

Let's throw in yet another resources in the mix to illustrate the point
further. Some part of the code may require a database connection:

```ocaml
let get_connection_string () :
    (string, [> `Connection_string of string Context.t ]) t =
  fetch ~tag:(fun ctx -> `Connection_string ctx)
```

We can now write a function that saves to a data-base and also does some logging:

```ocaml
(*
val store_item :
  string ->
  (unit,
  [> `Connection_string of string Context.t
   | `Log_mode of log_mode Context.t
   | `User_id of int Context.t ])
  t
 *)
let store_item item =
  let* connection = get_connection_string () in
  let* () = save_to_database ~connection item in
  log ("Saved item " ^ item)
```

Looking at the inferred signature of `store_item`, it's a function that given
a `string` value, returns a computation that produces a `unit` and requires
three resources:

- A connection-string of type `string`
- A log-mode of type `log_mode`
- A user-id or type `int`

Say our top-level program now calls `store_item`:

```ocaml
let program =
  let* () = store_item "My-item" in
  return ()
```

In order to run it, we have to supply all three dependencies, as in:

```ocaml
let run =
  program
  |> provide (function
      | `Connection_string ctx -> Context.value "abc123" ctx
      | `User_id ctx -> Context.value 123 ctx
      | `Log_mode ctx -> Context.value Local ctx)
  |> run
 ```

## Modules as dependencies

Nothing prevents us from extending resources to also include (first-class)
modules. Consider a simple example for factoring out a logging module
of type:

 ```ocaml
module type Logging = sig
  val log : string -> unit
end
```

We first define a `log` function for fetching and using the module:

```ocaml
(* val log : string -> (unit, [> `Logging of (module Logging) Context.t ]) t) *)
let log s =
  let* lm = fetch ~tag:(fun ctx -> `Logging ctx) in
  let module L = (val lm : Logging) in
  L.log s;
  return ()
```

Here's how to use and run it:

```ocaml
module ConsoleLogger = struct let log = print_endline end

let _ =
  run @@
    provide
      (function `Logging ctx -> Context.value (module ConsoleLogger : Logging) ctx)
      program
```

## Solving the *three-module problem*

Many times a particular resource, such as a logging module, is used in
multiple places -- also by modules that are themselves exposed as resources.
This can be illustrated by what I here call the *three-module problem*. Say
we have the following component interfaces for which we wish to
parameterize our code on:

```ocaml
module type Logging = sig
  val log : string -> unit
end

module type Database = sig
  val query : string -> unit
end

module type Application = sig
  val app : unit -> unit
end
```

Assume that to register and run a service, a module-functor `MakeService` is
given. It needs a logging module as well as an application:

```ocaml
module MakeService (L : Logging) (A : Application) = struct
  let run _ =
    L.log "Register service";
    A.app ()
end
```

An `Application` may also be parameterized by `Logging` and `Database`
modules, why we define it as another module-functor:

```ocaml
module MakeApplication (L : Logging) (D : Database) = struct
  let app () =
    L.log "Staring app";
    D.query ".."
end
```

To run a service we need concrete instances:

```ocaml
module Logger = struct let log = print_endline end
module Database = struct let query _ = () end
module Application = MakeApplication (Logger) (Database)
```

From which the service module may be instantiated:

```ocaml
module Service = MakeService (Logger) (Application)
```

However, note that we've now supplied the `Logging` module twice -- ones
for building the `Application` and once for building the `Service`.
Nothing prevents us from using two different implementations, like so:

```ocaml
module RemoteLogger = struct .. end
module Service = MakeService (RemoteLogger) (Application)
```

Here the application logging will be using the `Logger` module while
top-level `Service` will be using `RemoteLogger`. There are other ways around
this such as embedding the `Logging` module inside the `Application` module
or parameterizing the `MakeService` functor on a functor for building the
`Appplication` rather than a concrete application. Neither of these options
are great IMO.

Let's look at how the resource pattern above accommodates for this.

We may still expose the two low-level module signatures:

```ocaml
module type Logging = sig
  val log : string -> unit
end

module type Database = sig
  val query : string -> unit
end
```

We also provide *accessors* for fetching these resources from the environment:

```ocaml
(* val log string -> (unit, [> `Logging of (module Logging) Context.t ]) t *)
let log s =
  let* lm = fetch ~tag:(fun ctx -> `Logging ctx) in
  let module L = (val lm : Logging) in
  return @@ L.log s

(* val string -> (unit, [> `Database of (module Database) Context.t ]) t *)
let query s =
  let* db = fetch ~tag:(fun ctx -> `Database ctx) in
  let module Db = (val db : Database) in
  return @@ Db.query s
```

Rather than exposing a `MakeService` module-functor, we can now provide it as
an ordinary function:

```ocaml
(*
  val service :
    ('a, [> `Logging of (module Logging) Context.t ] as 'b) t ->
    ('a, 'b) t
*)
let service app =
  let* () = log "Register service" in
  app
```

And same for the `MakeApplication` functor, turning it into a function:

```ocaml
(*
  val app :
    unit ->
    (unit,
    [> `Database of (module Database) Context.t
      | `Logging of (module Logging) Context.t ]) t
*)
let app () =
  let* () = log "Starting app" in
  query "..."
```

We are now able to create a service, via:

```ocaml
(*
  val my_service :
  (unit,
  [ `Database of (module Database) Context.t
  | `Logging of (module Logging) Context.t ]) t
*)
let my_service = service @@ app ()
```

To construct a *runnable* instance , we supply implementations
for the low-level modules and use the *provide* function, as explained above:

```ocaml
module Logger = struct let log = print_endline end
module Database = struct let query _ = () end

let program : (unit, void) t=
  my_service
  |> provide (function
    | `Logging ctx -> Context.value (module Logger : Logging) ctx
    | `Database ctx -> Context.value (module Database : Database) ctx)
```

Note that all the resource dependencies are *flattened*! The `Logging` module
is used both by the `service` function and by the `app ()` computation, but
they are aggregated into a single dependency.

For instance running `my_service` in a test context, with mock implementations is
a matter of providing alternative implementations, as in:

```ocaml
module MockLogger = struct .. end
module MockDatabase = struct .. end

let test_program : (unit, void) t=
  my_service
  |> provide (function
    | `Logging ctx -> Context.value (module MockLogger : Logging) ctx
    | `Database ctx -> Context.value (module MockDatabase: Database) ctx)
```

## Implementation

Extending the reader-monad to implement the proposed API above is
straight-forward for the most part. The challenging bit is the
definition of `fetch` which curiously involves a solution to the problem
posted [here](https://blog.janestreet.com/a-universal-type/).

I've put the complete code along with the examples above in [this
gist](https://gist.github.com/jobjo/15e687c95e9a2a40c8f15b3142b4516a).