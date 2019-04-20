---
layout: post
title: OCaml lenses via modules
---
Lenses, often described as first class getters and setters, can help
simplify code for manipulating nested data structures. In this
post I'm going to look at how to map the most popular Haskell representation,
[van Laarhoven lenses](https://www.twanvl.nl/blog/haskell/cps-functional-references),
to OCaml.

### Lenses ala van Laarhoven
I won't cover lenses in Haskell but a good starting point is [this
talk](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation)
by Simon Peyton Jones. Following is the the basic definition of a lens, first proposed by
[Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/cps-functional-references):

```haskell
type Lens a b = forall f. Functor f => (b -> f b) -> (a -> f a)
```

As we shall see, the curious thing is that this data type embeds
a *getter* for extracting values as well as a *setter* for
updating a value. This is possible by varying the choice of the `Functor` on
the call site.

### Mapping to OCaml
The Haskell definition above is not directly translatable to OCaml due to
the lack of higher-kinded polymorphism. That is, the following attempt does not quite
work:

```haskell
(* Invalid *)
type ('a,'b) lens = ('b -> 'b f ) -> ('a -> 'a f)
```

A general scheme for working around this limitation is to turn to the module
system. First we need a representation of Haskell *functors*. In Haskell, a
`Functor`, is a type class with a function `map`:

```haskell
class  Functor f  where
  fmap :: (a -> b) -> f a -> f b
```

In OCaml this can be represented using a module signature:

```haskell
module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
```

For a more comprehensive discussion on how to map type classes in Haskell
to modules in OCaml, see [this post](http://blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html).

The lens type itself can be achieved by another module type for wrapping
the type parameters along with a module functor for constructing the
the lens function given any concrete `FUNCTOR` implementation:

```ocaml
module type LENS = sig
  type a
  type b
  module Mk : functor (F : FUNCTOR) -> sig
    val run : (b -> b F.t) -> a -> a F.t
  end
end
```

In other words - a *lens* from `a` to `b` is a module that provides a
constructor (`Mk`) for building another module that exposes a function `run`
defining the lens.

To bridge the gap between modules and types one can also create a type
alias:

```ocaml
type ('a,'b) lens = (module LENS with type a = 'a and type b = 'b)
```

and a function for simplifying construction of lens values:

```ocaml
let mk_lens (lens : (module LENS with type a = 'a and type b = 'b))  : ('a, 'b) lens = lens
```

Before looking at how to actually use lenses for extracting or setting values,
let's consider an example instance. Say we have the following types:

```ocaml
type address = { street : string ; number : int; postcode : string }
type person = { name : string; age : int; address : address }
```

Below is a lens pointing to the `address` property of a `person`:

```ocaml
let address =
  mk_lens (
    module struct
      type a = person
      type b = address
      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun address -> { x with address }) @@ f x.address
      end
    end
  )
```

The interesting bit is the definition of `run` which takes arguments `f` and `x`,
where:

```ocaml
val f : address -> address F.t
val x : person
```

Note that `F` is an arbitrary `FUNCTOR`. There's really only one possible
(non-trivial) implementation of run given these constraints in general.
Definitions of lenses corresponding to properties is mostly boilerplate
and should rather be automated (for instance via *ppx deriving*).

### Modifying values

To see why this is useful at all, let's look at how to apply lenses
for modifying or setting values. Lens libraries typically provide a function
`modify` equivalent to the following signature:

```ocaml
val modify : ('a, 'b) lens -> ('b -> 'b) -> 'a -> 'a
```

Using the `address` lens from above as an example, we have:

```ocaml
modify address : (address -> address) -> person -> person
```

For this purpose we ultimately need the `run` function of a lens to be identical to:

```ocaml
let run f x = { x with address = f x.address }
```

This can be achieved by picking a functor that does not do
anything besides applying the argument; The so called identity functor:

```ocaml
module IdFunctor : FUNCTOR with type 'a t = 'a = struct
  type 'a t = 'a
  let map f x = f x
end
```

To see why exactly this works out:

```ocaml
let run f x = F.map (fun address -> { x with address }) @@ f x.address =
(* Definition of map for IdFunctor *)
(fun address -> { x with address }) @@ f x.address
(* Beta reduction *)
{ x with address = f x.address }
```

Putting the pieces together, here's the implementation of a function `modify`:

```ocaml
let modify  (type u)
            (type v)
            (lens : (module LENS with type a = u and type b = v))
            (f : (v -> v))
            (x : u) =
  let module L = (val lens) in
  let module R = L.Mk (IdFunctor) in
  R.run f x
```

As an example, let's define another lens referencing the `postcode` property of an address:

```ocaml
let postcode =
  mk_lens (
    module struct
      type a = address
      type b = string
      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun postcode -> { x with postcode }) @@ f x.postcode
      end
    end
  )
```


Here's how to use `modify` to map over the postcode:

```ocaml
let address = { street = "Highstreet"; number = 42; postcode = "e1w" };;

modify postcode String.uppercase_ascii address;;
- : address = {street = "Highstreet"; number = 42; postcode = "E1W"}
```

For completeness one should also include a special case of `modify`
for replacing a value:

```ocaml
let set lens x = modify lens (fun _ -> x)
```

where

```ocaml
val set : ('a,'b) lens -> 'b -> 'a -> 'a
```

Here's an example that *sets* the postcode of the address value above:

```ocaml
set postcode "XYZ" address;;
- : address = {street = "Highstreet"; number = 42; postcode = "XYZ"}
```

### Extracting values
Lenses can also be used for extracting or *viewing* the values
pointed to - we are aiming for a function with the following
signature:

```ocaml
val view : ('a, 'b) lens -> 'a -> 'b
```

Extracting values using lenses involves some cleverness in terms
of picking the right functor. For example, to get the address from a
person via the `address` lens from above, we to tweak the run function.
Looking at its defintion again:
```ocaml
let run f x = F.map (fun address -> { x with address }) @@ f x.address
```

We need `run` to evaluate to `x.address`. The trick is to pick a functor that
ignores the function argument and returns a constant value:

```ocaml
module type TYPE = sig type t end
module ConstFunctor (T : TYPE) : FUNCTOR with type 'a t = T.t = struct
  type 'a t = T.t
  let map _ x = x
end
```

With `ConstFunctor` at our disposal a function `view` can be accomplished with:

```ocaml
let view  (type u)
          (type v)
          (lens : (module LENS with type a = u and type b = v))
          (x : u) =
  let module L = (val lens) in
  let module R = L.Mk (ConstFunctor (struct type t = v end)) in
  R.run (fun x -> x) x
```

To look at how this adds up, simply expand the definition of `R.run`.

Here's an example of how to use `view`:

```ocaml
view postcode { street = "Highstreet"; number = 42; postcode = "e1w" };;
- : string = "e1w"
```

### Composing lenses
There would be little point in defining lenses if it weren't for the
ability to compose them. We can either define composition as a module functor
or via a function inlining the module construction. Here's the latter version:

```ocaml
let compose (type u)
            (type v)
            (type x)
            (l1 : (module LENS with type a = v and type b = x))
            (l2 : (module LENS with type a = u and type b = v)) =
  mk_lens (
    module struct
      type a = u
      type b = x
      module L1 = (val l1)
      module L2 = (val l2)
      module Mk (F : FUNCTOR) = struct
        module R1 = L1.Mk (F)
        module R2 = L2.Mk (F)
        let run f x = R2.run (R1.run f) x
      end
    end
  )
```

To fully appreciate the power of `compose` consider the infix version:

```ocaml
val (//) : ('a, 'b) lens -> ('c, 'd) lens -> ('a, 'c) lens
```

As an example, say we have a value `person`:

```ocaml
let mary =
  {
    name = "Mary";
    age = 33;
    address = { street = "Highstreet"; number = 42 ; postcode = "e1w"}
  }
```

Viewing or updating the postcode code of `mary` is straigt forward:

```ocaml
view (address // postcode) mary;;
:- "e1w"

update (address // postcode) String.uppercase_ascii mary;;
:- { name = "Mary"; age = 33; address =
        { street = "Highstreet"; number = 42 ; postcode = "E1W"} }

set (address // postcode) "XYZ" mary;;
:- { name = "Mary"; age = 33; address =
        { street = "Highstreet"; number = 42 ; postcode = "XYZ"} }

```
