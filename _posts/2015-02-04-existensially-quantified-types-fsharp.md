---
layout: post
title: Church Encoding and Existentially Quantified types in F#
---
What would happen is the F# team decided to drop the support for algebraic data types (discriminated unions)? Certainly not a pleasant thought but perhaps just not as horrifying as you might expect.

Consider for instance the standard option type:

{% highlight fsharp %}
type Option<'T> =
  | Some of 'T
  | None
{% endhighlight %}

and the ability to pattern match over `Option` values:

{% highlight fsharp %}
let myFunction (p: Option<'T>) =
  match p with
  | Some x  -> ...
  | None    -> ...
{% endhighlight %}

How were we to define an isomorphic data type without using a sum-type?

One solution is given by [church-encodings](http://en.wikipedia.org/wiki/Church_encoding); A data structure can be encoded as a function accepting one continuation per constructor. For instance consider a function 

{% highlight fsharp %}
val someOptionValue<'T> : (unit -> 'T) -> (int -> 'T) -> 'T
{% endhighlight %}

Thinking of implementation of this function it's clear that it can only do one of two things; Either invoke the first argument with a unit value or invoking the second function with a particular integer. It is straight-forward to convert it back to a regular `option<int>`:

{% highlight fsharp %}
myOption (fun _ -> None) Some
{% endhighlight %}

We can further define functions `fromOption` and  `toOption` and show that they are each others inverse:

{% highlight fsharp %}
let fromOption<'T,'U> : option<'T> -> (unit -> 'U) -> ('T -> 'U) -> 'U = function
    | Some x    -> fun _ s -> s x
    | None      -> fun n _ -> n ()

let toOption op = op (fun _ -> None) Some
let id<'T> = fromOption<'T,option<'T>> >> toOption

{% endhighlight %}

We can also mimic the constructors of the original `Option` type:

{% highlight fsharp %}
// Constructs an empty value.
let none = fun n f -> n ()

// Constructs some value
let some x = fun _ f -> f x
{% endhighlight %}

as well as defining a function for mapping over optional values:

{% highlight fsharp %}
// Map over an option value.
let map f op = fun n s -> op n (fun x -> s (f x))

// Example - Evaluates to Some 42
toOption <| map ((+) 1) (some 42)

// Evaluates to None
toOption <| map ((+) 1) none
{% endhighlight %}

In order to package it up as a library it would be nice to create a type synonym for the church-encoded option type. How about simply wrapping the function inside a record?

{% highlight fsharp %}
type Option<'T> = internal {Run<'U> : ((unit -> 'U) -> ('T -> 'U) -> 'U)}
{% endhighlight %}

This doesn't quite work since F# records don't support polymorphic fields. That is, each type variable must be listed on the left hand side of its type definition.It would be silly to require each option value to be parameterized by the return type of a function running it. What is required here is instead an [existentially quantified type](https://downloads.haskell.org/~ghc/5.00/docs/set/existential-quantification.html). Luckily we can achieve this by simply switching to standard interfaces:

{% highlight fsharp %}
type Option<'T> = abstract member Run : (unit -> 'U) ->  ('T -> 'U) ->  'U
{% endhighlight %}

and modifying the utility functions accordingly:

{% highlight fsharp %}
let run (o: Option<'T>) = o.Run
let none<'T> = {new Option<'T> with member this.Run n s = n () }
let some (x: 'T) = {new Option<'T> with member this.Run _ s = s x }
let map (f: 'T -> 'U) (op: Option<'T>) =  {new Option<'U> with member this.Run n s = op.Run n (f >> s) }
{% endhighlight %}

Finally here are some examples of how to work with this library:

{% highlight fsharp %}
// Example values.
let v0 = none<int>
let v1 = some 32
let v2 = map ((+) 1) v1

// Prints the result of an option value.
let printResult (v: Option<'T>) =
    run v
        (fun _ -> printfn "None")
        (fun x -> printfn "Value %A" x)

printResult v0 // Prints `None`
printResult v1 // Prints `Value 32`
printResult v2 // Prints `Value 33`
{% endhighlight %}

Looking at the `printResult` function the similarities to ordinary pattern matching over algebraic data types is striking.














