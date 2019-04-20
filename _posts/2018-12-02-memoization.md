---
layout: post
title: Memoization
---

Memoization is a strategy for preventing values to be computed multiple
times. The sledgehammer approach in OCaml is a function with the signature:

```ocaml
val memoize : ('a -> 'b) -> 'a -> 'b
```

That is, `memoize` extends any given function with *memory* so
that anytime it's called with the same input, it's going to return a cached result.

Whether or not one should apply this technique depends on the call patterns
and execution costs. It may be justified to memoize a function when:

1. It is pure, or at least yields deterministic results.
2. Is expensive to evaluate.
3. Is going to be called multiple times with the same arguments.
4. The memory overhead of caching the results is acceptable.

## Using a mutable cache

The school book implementation is a function returning a closure that
keeps an internal dictionary for storing previously calculated values:

```ocaml
let memoize f =
  let cache = Hashtbl.create 256 in
  fun x ->
    match Hashtbl.find_opt cache x with
    | Some x  ->
      x
    | None    ->
      let y = f x in
      Hashtbl.add cache x y;
      y
```

To address item (4) from above the implementation may be adjusted
to constrain the maximum size of the cache.

## Going pure

From a theoretical point of view, would it be possible to implement
`memoize` in a purely functional way?

The answer is, *almost*. We can solve the problem by first constructing a table
with key value pairs of all arguments along with their calculated values.
For that to work we're going to have to restrict the set of memoizable functions a
bit. Note also that the hash-table version above is not quite as
generic as it appears. Since it relies on built in comparison it fails
at runtime on key collision for keys that are not *comparable*. With a pure
version we need to deal with keys that are not only comparable but also
enumerable. For simplicity let's only consider functions from integers (knowing
that it can be generalized):

```ocaml
val memoize : (int -> 'a) -> int -> 'a
```

So given a function of some type `(int -> 'a)`, the game plan is to construct
a pure map containing all integers and corresponding values upfront. We then
return a function that looks up values from the map. Something like:

```ocaml
(* Does not quite work *)
let memoize f =
  let map =
    IntMap.of_list @@
      List.fold_left (IntMap.add x (f x)) all_ints
  in
  fun x -> IntMap.lookup x map
```

The crux is of course that we're going to get stuck on building the initial map.

The trick is to be *lazy*. We can set up the structure of the map but only
fill it with values when need be. For that purpose here's a lazy *trie*:

```ocaml
type 'a trie = {
  key   : int;
  value : 'a Lazy.t;
  left  : 'a trie Lazy.t;
  right : 'a trie Lazy.t
}
```

Only the `key` property in this structure is strict. The value and left and
right branches are computed on demand. The following function constructs a
`trie` balanced around `0`:

```ocaml
let make f =
  let rec aux min max =
    let n = (min + max) / 2 in
    { key   = n
    ; value = lazy (f n)
    ; left  = lazy (aux min n)
    ; right = lazy (aux n max)
    }
  in
  aux min_int max_int
```

Looking up values from a `trie` is straight forward:

```ocaml
let rec lookup { key; value; left; right } n =
  if key = n then
    Lazy.force value
  else if n < key then
    lookup (Lazy.force left) n
  else
    lookup (Lazy.force right) n
```

With `make` and `lookup` memoization is achieved by:

```ocaml
let memoize f = lookup @@ make f
```

To convince ourselves that it actually works, consider this example:

```ocaml
let plus_one n =
  print_endline "I am a side effect!";
  Unix.sleep 2;
  n + 1

let plus_one' = memoize plus_one
```

Calling `plus_one'` repeatedly with the same argument only evaluates the
embedded function once:

```ocaml
> plus_one' 42;;
I am a side effect!
:- int = 43

> plus_one' 42;;
- : int = 43
```

## What about recursion?

Irregardless of the implementation, can we also use `memoize` as an
optimization strategy for recursive functions? Can we write an efficient
*fibonacci*?

First let's consider what does not work:

```ocaml
(* Exponential complexity *)
let rec fib n =
  if n <= 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

let fib' = memoize fib
```

The problem here is that memoization only applies at the top level.
Evaluating fibonacci the first time is still slow. We need to somehow be
able to embed the memoization into the recursive calls. How about the
following?

```ocaml
let rec fib n =
  if n <= 1 then
    1
  else
    let fib' = memoize fib in
    fib' (n - 1) + fib' (n - 2)
```

Still not good as we're not actually reusing the memoized version across the
recursive calls. Instead we're building several memoized functions, each with
its own cache.

To work around this, let's first break the recursion by parameterizing the `fib`
function by a function replacing the recursive call:

```ocaml
let fib f n =
  if n <= 1 then
    1
  else
    f (n - 1) + f (n - 2)
```

The inferred type is:

```ocaml
val fib : (int -> int) -> int -> int = <fun>
```

Second, we need a [fix-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator)
for tying the knot. An implementation of the standard version is:

```ocaml
let fix f =
  let rec aux = lazy (fun x -> f (Lazy.force aux) x) in
  Lazy.force aux
```


Where `fix` has the following, and perhaps not the most intuitive, signature:

```ocaml
val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
```

Equipped with `fix`, an alternative non-recursive `fib` may be defined as:

```ocaml
let fib = fix @@ fun fib n ->
  if n <= 1 then
    1
  else
    fib (n - 1) + fib (n - 2)
```

The final piece of the puzzle is to adjust the implementation of `fix` to
also perform memoization:

```ocaml
let fix f =
  let rec aux = lazy (memoize @@ fun x -> f (Lazy.force aux) x) in
  Lazy.force aux
```

The definition of `fib` stays the same but the complexity is linear!

```ocaml
> fib 100;;
- : int = 1298777728820984005
```