---
layout: post
title: Implicit functors
---
Modular implicits is an experimental feature of OCaml that has yet to land
on the master branch. In this and upcomings posts I'm going to give a few
examples of what it brings to the table. For an introduction to the topic it's
best to read [the original
paper](https://www.cl.cam.ac.uk/~jdy22/papers/modular-implicits.pdf).

To run the code below I'm using a branch of the OCaml compiler available via:

{% highlight ocaml %}
opam switch 4.02.0+modular-implicits
eval `opam config env`
{% endhighlight %}

At first we'll take a look at functors, that is the *functor pattern*
and not module functors. A functor, as inspired by the Haskell type class,
can be represented in OCaml by a module signature:

{% highlight ocaml %}
module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
{% endhighlight %}

The signature states that a functor is a module with some type `'a t` along
with a `map` function. Before looking at any concrete instances, we can already
make use of modular implicits and define the following (generic) map function:

{% highlight ocaml %}
let map {F : FUNCTOR} = F.map
{% endhighlight %}

Here, `map` is a function that takes an argument `F` of type `FUNCTOR` module and
applies its map function. It has the following signature:

{% highlight ocaml %}
val map : {F : FUNCTOR} -> ('a -> 'b) -> 'a F.t -> 'b F.t
{% endhighlight %}

The interesting bit is that the instance of `F` may be deduced
by the context in which `map` is invoked and applied implicitly. 
For example:

{% highlight ocaml %}
map String.length ["list"; "of"; "strings"];;
map sqrt (Some 9.);;
{% endhighlight %}

Both these invocations of map are valid given that there exists in scope *implicit*
modules implementing the `FUNCTOR` signature for the `list` and `option` types:

{% highlight ocaml %}
implicit module ListFunctor = struct
  type 'a t = 'a list
  let map f = List.map f
end

implicit module OptionFunctor = struct
  type 'a t = 'a option
  let map f = function
    | Some x  -> Some (f x)
    | None    -> None
end
{% endhighlight %}

In addition to these concrete instances, it is also possible to define
instances inductively. For example by introducing an implicit module
defining the functor interface for compositions or functors:


{% highlight ocaml %}
implicit module ComposeFunctor {A : FUNCTOR} {B : FUNCTOR } = struct
  type 'a t = ('a B.t) A.t
  let map f = A.map (B.map f)
end
{% endhighlight %}

That is to say, any two functors composed, for instance a list of options, is also
a functor! Mapping is achieved by mapping over both the inner and the outer
structures. Here are a couple of examples:

{% highlight ocaml %}
map String.length [Some "foo"; None; Some "bar"];;
map String.length (Some ["a"; "bc"; "def"]);;
{% endhighlight %}

Similarly, the product of two functors also forms a functor:

{% highlight ocaml %}
implicit module ProdFunctor {A : FUNCTOR} {B : FUNCTOR } = struct
  type 'a t = 'a A.t * 'a B.t
  let map f (x,y) = (A.map f x, B.map f y)
end
{% endhighlight %}

This enhances the universe of mappable structures further, for instance:

{% highlight ocaml %}
map String.length ([Some "apple"; None], Some "pear");;
{% endhighlight %}

The reason the examples above type check is because the machinery around
modular implicits is able derive the functor by first considering
the product functor and secondly the list composition functor with
list and option for the first component and the option functor for the second.
Without the implicitness one would have to manually do the plumbing, as in:


{% highlight ocaml %}
module F = ProdFunctor{ComposeFunctor{ListFunctor}{OptionFunctor}}{OptionFunctor}
map {F} String.length ([Some "apple"; None], Some "pear");;
{% endhighlight %}

What's also worth noticing is that any variation of nested functors or product
of functors are guaranteed by induction to be well behaved, that is satisfying 
the functor laws:

{% highlight ocaml %}
map (fun y -> y) x  =  x
map f (map g x)     = map (fun y -> f (g y)) x
{% endhighlight %}

This is of course conditional on having valid concrete functors at the bottom of the stack.
For some more background on functors laws
with OCaml examples, see [this post](blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html).

There are some interesting applications of these concepts in the context of generic
programming, a topic which I hope to come back to. 

For now, here is the [complete code](https://gist.github.com/jobjo/41cf3d9d9d5674db32f40afdbf29df18)
from the examples above.
