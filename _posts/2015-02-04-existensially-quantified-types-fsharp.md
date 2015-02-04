---
layout: post
title: Church Encoding and Existentially Quantified types in F#
---
What would happen is the F# team decided to drop the support for algebraic data types (discriminated unions)? Certainly not an encouraging thought but perhaps just not as horrifying as you might expect.

Consider how the familar option type is defined in F#

{% highlight fsharp %}
type option<'T> =
  | Some of 'T
  | None
{% endhighlight %}

and the ability to pattern match over `Option` values:

{% highlight fsharp %}
let foo =
  match someValue with
  | Some x  -> ...
  | None    -> ...
{% endhighlight %}

Is it possible to define an isomorphic data type without using an algebraic data type?

One solution is given by [church-encodings](http://en.wikipedia.org/wiki/Church_encoding); A data structure can be encoded as a function accepting one continuation per constructor. For instance consider a function 

{% highlight fsharp %}
val someOptionValue<'T> : (unit -> 'T) -> (int -> 'T) -> 'T
{% endhighlight %}

When thinking of the possible implementations of this function it's clear that it can only do one of two things; Either invoke the first argument with a unit value or apply the second argument on some integer. It is straight-forward to convert the value to regular `option<int>`.

{% highlight fsharp %}
myOption (fun _ -> None) Some
{% endhighlight %}

We can further define functions `fromOption` and  `toOption` and show that they are each others inverse:

{% highlight fsharp %}
// Church-encode an option value.
let fromOption = function
    | Some x    -> fun _ s -> s x
    | None      -> fun n _ -> n ()

// Decode a church-encoded option value.
let toOption op = op (fun _ -> None) Some

{% endhighlight %}

We can also mimic the constructors of the original `option` type:

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

This doesn't quite do the trick since F# records don't support polymorphic fields. That is, each type variable must be listed on the left hand side of its type definition.It would be silly to require each option value to be parameterized by the return type of a function running it. Instead the return type should be [existentially quantified](https://downloads.haskell.org/~ghc/5.00/docs/set/existential-quantification.html). Luckily all that is required is switching to standard interfaces:

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













