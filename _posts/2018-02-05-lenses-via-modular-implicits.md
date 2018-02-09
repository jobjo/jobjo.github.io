---
layout: post
title: Lenses via modular implicits
---
Following is a continuation of the topic of *modular implicits* , introduced in
the previous post on [implicit
functors](http://jobjo.github.io/2018/01/28/implicit-functors.html). This time
we'll look at how the extension can help simplifying *lenses*. I covered
lenses in [OCaml lenses via
modules](http://jobjo.github.io/2017/12/20/lenses-as-modules.html), where a
rather verbose definition of a (van Laarhoven) lens was given in the form of a
module signature `LENS`:

{% highlight ocaml %}
module type LENS = sig
  type a
  type b
  module Mk : functor (F : FUNCTOR) -> sig
    val run : (b -> b F.t) -> a -> a F.t
  end
end
{% endhighlight %}

With modular implicits, much of the clunkiness will go away. At first, let's
cover some ground and bring into scope a couple of utility functions - a module
type for representing *functors* and two functor instances (for identity and
constant):

{% highlight ocaml %}
let id x = x

let const x _ = x

module type TYPE  = sig type t end

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module IdFunctor = struct
  type 'a t = 'a
  let map f x = f x
end

module ConstFunctor (T : TYPE) = struct
  type 'a t = T.t
  let map _ x = x
end
{% endhighlight %}

All of the definitions are vanilla OCaml and were also described in
the [previous lens post](http://jobjo.github.io/2017/12/20/lenses-as-modules.html).

The reason the lens representation required a module signature rather than a
simple type is due to OCaml's inability to parameterize over higher-kinded
types. Powered by modular implicits however, there is a work around; We are now
able to define lens as a function type:

{% highlight ocaml %}
type ('a, 'b) lens = {F : FUNCTOR} -> ('b -> 'b F.t) -> 'a -> 'a F.t
{% endhighlight %}

Note that the implicit argument `F` is available in scope for other arguments as well
as the return type of the function. This is not achievable with normal first class
modules in OCaml. More specifically, the following type construction is invalid:

{% highlight ocaml %}
(* Does not compile :( *)
type ('a, 'b) lens = (F : FUNCTOR) -> ('b -> 'b F.t) -> 'a -> 'a F.t
{% endhighlight %}

Comparing with the previous version, the `view` and `modify` functions used for
extracting and updating values respectively are also simplified:

{% highlight ocaml %}
let view (type a) (type b) (l : (a, b) lens) (x : a) : b =
  let module C = ConstFunctor (struct type t = b end) in
  l {C} id x

let modify (type a) (type b) (l : (a, b) lens) (f  : b -> b) (x : a) : a =
  l {IdFunctor} f x
{% endhighlight %}

Viewing a lens is accomplished by using the `ConstFunctor`, instantiated
with the concrete type parameter `b` in order to smuggle out the value
that the lens is pointing to. The function `modify` instead relies on `IdFunctor`
for updating the value.

A utility, `set`,  is introduced for convenience:

{% highlight ocaml %}
let set l x = modify l (const x)
{% endhighlight %}

Since lenses are functions, lens composition is nothing but function composition:

{% highlight ocaml %}
let compose (l2 : ('b, 'c) lens) (l1 : ('a, 'b) lens) : ('a, 'c) lens = 
  fun { F : FUNCTOR } f x -> l1 {F} (l2 {F} f) x

let (//) l1 l2 = compose l2 l1
{% endhighlight %}

To see how the pieces fit together, let's take look at some concrete examples.
Consider the following custom types:

{% highlight ocaml %}
type address = { street : string ; number : int}

type person = { name : string; age : int; address : address }

type compnay = { name : string; ceo : person }
{% endhighlight %}

We first introduce lenses for some of the properties manually:

{% highlight ocaml %}
let ceo { F : FUNCTOR } f x = F.map (fun ceo -> { x with ceo }) @@ f x.ceo

let address { F : FUNCTOR } f x = 
  F.map (fun address -> { x with address }) @@ f x.address

let street { F : FUNCTOR } f x = 
  F.map (fun street -> { x with street }) @@ f x.street
{% endhighlight %}

Although an improvement over the module based approach,
lenses for record properties still require boiler plate code and should rather
be automated by a *deriving mechanism*.

Now, given a value of type `company`:

{% highlight ocaml %}
let my_company = {
  name = "Lens Inc";
  ceo = {
    name = "Mary";
    age = 62;
    address = { 
      street = "Highstreet";
      number = 13;
    }
  }
}
{% endhighlight %}

Using the lenses from above, here is how to update the street component
of `my_company` using a composed lens and the `set` function:

{% highlight ocaml %}
set (ceo // address // street) "Wallstreet" my_company;;
{% endhighlight %}

This code results in the following value:

{% highlight ocaml %}
{
  name = "Lens Inc";
  ceo = {
    name = "Mary"; 
    age = 62; 
    address = {
      street = "Wallstreet"; 
      number = 13
     }
  }
}
{% endhighlight %}

As you may have observed, there is not a single *implicit* module in the code
above - the value proposition of modular implicit goes beyond sparing
an extra argument to a function. 

Complete code [here](https://gist.github.com/jobjo/2223edc502e875b4305aae2a735baa62).
