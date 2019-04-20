---
layout: post
title: Church encoding products and coproducts
---

In a [previous
post](http://jobjo.github.io/2015/02/04/existensially-quantified-types-fsharp.html)
I gave an example of how to represent algebraic data types using church
encoding. In this post I'll pick up the thread and show how this technique can
be used to mimic any type.

There are two ways of constructing new types in functional programming
languages; Either by using a sum type or a product type. Products are
composite types such a records and tuples. Sum types are for representing data that can take
different shapes and are usually described using an algebraic data type
(know as discriminated unions in OCaml or FSharp).

### Products
In category theory, the universal property of products states that any two
products $$P$$ and $$Q$$ with the same set of projection arrows must have unique morphisms
between them such that any diagram of the following type commutes:

$$
\begin{array}[c]{ccccc}

T_1 & \leftarrow & P        & \rightarrow  & T_2 \\
    & \nwarrow   & \uparrow \downarrow  & \nearrow    \\
    &            & Q

\end{array}
$$

To give a concrete example, in the category of types and functions in ML, the
tuple type `int * string` can be safely substituted with a record type:


```ocaml
type person = {name : string; age : int}
```

Proving this is trivial, one simply needs to define a function for converting
a tuple to a person:

```ocaml
let to_person (name,age) = { name = name; age = age }
```

And show that:

```ocaml
(to_person t).name = fst t
(to_person t).age  = snd t
```

`name`, `age`, `fst` and `snd` are the projection arrows. The equations
expresses commutativity in one direction.

Products of more than two components can be expressed as nested binary
products. For instance a three component product: $$A \times B \times C$$ is
isomorphic to $$A \times (B \times C)$$.

The following signature captures all there is to know about products,
abstracting over the concrete type:

```ocaml
module type Product = sig
    (* The type of the product. *)
    type ('a, 'b) product

    (* Creates a product given the components.*)
    val mk : 'a -> 'b -> ('a, 'b) product

    (* Extracts the first component. *)
    val fst : ('a, 'b) product -> 'a

    (* Extracts the second component. *)
    val snd : ('a, 'b) product -> 'b
end;;
```

First, let's implement the interface using tuples:

```ocaml
module Tuple_Product : Product = struct
    type ('a, 'b) product = 'a * 'b
    let mk x y = (x,y)
    let fst (x,_) = x
    let snd (_,y) = y
end;;
```

Can this interface be realized with simple functions, i.e. without resorting
to tuples or records for representing multiple properties? Church encoding
demonstrates that the answer is yes! A product type is isomorphic to a higher
order function where the function parameter accepts one argument per component
of the product. To give a concrete example, the product `'a * 'b` can be
encoded as a function `'('a -> 'b -> 'c) -> 'c`. It's clear that in order to
return a value of type `'c`, the function must invoke the continuation,
passing it an `'a` and a `'b` value. The particular values are captured within the closure of the
function. Here is the complete church implementation of the `Product`
signature:

```ocaml

module Church_Product : Product = struct
    type ('a , 'b) product = { run : 'c. ('a -> 'b -> 'c) -> 'c}
    let mk x y = { run = fun f -> f x y }
    let fst p = p.run @@ fun x _ -> x
    let snd p = p.run @@ fun _ y -> y
end;;
```

A nice thing with OCaml, comparing with F#, is the support for polymorphic
record properties (here needed for `run`).

### Coproducts
Coproduts in category theory look like products with all arrow reversed.
Instead of having arrows from a coproduct to other objects there are morphisms
from other objects to the coproduct. The following diagram depicts two
coproducts $$C$$ and $$U$$:

$$
\begin{array}[c]{ccccc}

T_1 & \rightarrow & C        & \leftarrow  & T_2 \\
    & \searrow   & \uparrow \downarrow  & \swarrow    \\
    &            & U

\end{array}
$$

Just like with products, the diagram must commute. In functional programming,
each arrow to coproduct type can be represented by a constructor of an
algebraic data type. For example:

```ocaml
type contact =
    | Email of string
    | Address of address
    | Phone of string
```
corresponds to two morphisms (`string -> contact`), for the `Email` and
`Phone` constructors and another (`address -> contact`) for the `Adress`
constructor.

Again, sum types with more than two constructors can be achieved by nesting,
so in order to define a generalized signature we only need to worry about the
binary case:

```ocaml
module type Coproduct = sig
    (* Type representing coproducts. *)
    type ('a, 'b) coproduct

    (* Constructs a value of the first shape. *)
    val left : 'a -> ('a, 'b) coproduct

    (* Constructs a value of the second shape. *)
    val right : 'b -> ('a, 'b) coproduct

    (* Runs a value given two continuations. *)
    val run : ('a, 'b) coproduct -> ('a -> 'c) -> ('b -> 'c) -> 'c
end;;
```

Following is an implementation using discriminated unions:

```ocaml
module DU_Coproduct = struct
    type ('a, 'b) coproduct = | Left of 'a | Right of 'b
    let left x = Left x
    let right x = Right x
    let run cp l r =
        match cp with
        | Left x  -> l x
        | Right x -> r x
end;;
```

A church encoded representation of a coproduct type with two constructors is
given by a function accepting one continuation per constructor. A function of
type `'('a -> 'c) -> ('b -> 'c) -> 'c`, can invoke either one of the passed
arguments and is equivalent to a discriminated union type with two cases.

Here is a church encoded version of the coproduct interface:

```ocaml
module Church_Coproduct : Coproduct = struct
  type ('a, 'b) coproduct =
    { run : 'c. ('a -> 'c) -> ('b -> 'c) -> 'c }
  let left x = { run = fun l _ -> l x }
  let right x = { run = fun _ r -> r x }
  let run u l r = u.run l r
end;;
```

The `left` constructor creates a function that only invokes the first
continuation while `right` returns a function that only invokes the second
continuation with the given argument.

### Implementing lists
To see how the generalized interfaces for product and coproduct can be used,
consider defining a simple list structure. First look at the standard
implementation of lists using a discriminated union:


```ocaml
let 'a list =
    | Nil
    | Cons of ('a * 'a list)
```

This can be translated to the following type in terms of the `Product` and
`Coproduct` signatures:

```ocaml
type 'a ch_list =
    { ext : (unit, ('a, 'a ch_list) product) coproduct }
```

The unit type is used to represent the `Nil` case. The non-empty case consists
of a product with value of type `a` and the rest of the list.

In order to simplify the creation of lists, here are a couple of helper
functions:

```ocaml

(* Helper for construct a list. *)
let mk_list v = { ext = v }

(* Empty list constructor. *)
let empty = mk_list @@ left ()

(* Cons list constructor.  *)
let cons x xs = mk_list @@ right @@ mk x xs
```

`empty` corresponds to `[]` in Ocaml and `cons` is equivalent to `(::)`.

We can also define a utility method for simulating pattern matching:

```ocaml
let rec match_list lst nil cons =
  run lst.ext
    nil
    (fun p -> cons (fst p) (snd p))
```

With these definitions, the standard `List.map` can be ported to:

```ocaml
let rec map f xs =
  match_list xs
    (fun _    -> empty)
    (fun x xs -> cons (f x) (map f xs))
```

A better implementation would use tail recursion but the principal is the same as with
ordinary lists.

At last, following are the conversion functions required for showing that the two
versions of lists are indeed isomorphic:

```ocaml

(* Convert to ordinary list. *)
let rec to_list lst =
  match_list lst
    (fun _    -> [])
    (fun x xs -> x :: (to_list xs))

(* Convert from ordinary list. *)
let rec from_list = function
  | []      -> empty
  | x :: xs -> cons x (from_list xs)
```

