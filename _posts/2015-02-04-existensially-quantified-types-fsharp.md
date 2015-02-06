---
layout: post
title: Church Encoding and Existentially Quantified types in F#
---
What would happen if the next F# compiler dropped support for discriminated unions (algebraic data types)? Not a pleasing thought but perhaps just not as horrifying as you might expect. For example, consider how the familiar option type is defined in F#

{% highlight fsharp %}
type option<'T> =
  | Some of 'T
  | None
{% endhighlight %}

along with the ability to pattern match over `Option` values:

{% highlight fsharp %}
match someValue with
| Some x  -> ...
| None    -> ...
{% endhighlight %}

Is it possible to construct an isomorphic data type without discriminated unions?

One solution is given by [church-encodings](http://en.wikipedia.org/wiki/Church_encoding); A data structure can be encoded as a function accepting one continuation per constructor. For instance, given a function

{% highlight fsharp %}
val optionValue<'T> : (unit -> 'T) -> (int -> 'T) -> 'T
{% endhighlight %}

what are its possible implementations? There are only two sensible things it may do; Either invoke the first argument with a unit value or apply the second argument on some integer. It's straight forward to find out which one by converting it to a regular `option<int>`.

{% highlight fsharp %}
let result : option<int> = optionValue (fun _ -> None) Some
{% endhighlight %}

Relaxing the constraint of using `int`, the generalized type `(unit -> 'T) -> ('U -> 'T) -> 'T` can be used to represent optional values of arbitrary type. The following definitions show that it's possible to translate back and forth between regular option types and its church-encoded counterpart.

{% highlight fsharp %}
// Church-encode an option value.
let fromOption = function
    | Some x    -> fun _ s -> s x
    | None      -> fun n _ -> n ()

// Decode a church-encoded option value.
let toOption op = op (fun _ -> None) Some
{% endhighlight %}

In order to ensure that the church option type is isomorphic to the regular version, one also needs to show that `fromOption >> toOption = id`. Simple equational reasoning does the trick. Both the cases `None` and `Some x` must be considered:

{% highlight fsharp %}

// Identity for None
(fromOption >> toOption) None               =
toOption (fromOption None)                  =
toOption (fun n _ -> n ())                  =
(fun n _ -> n ()) (fun _ -> None) Some      =
(fun _ -> None) ()                          =
None

// Identity for Some x
(fromOption >> toOption) (Some x)           =
toOption (fromOption (Some x))              =
toOption (fun _ s -> s x)                   =
(fun _ s -> s x) (fun _ -> None) Some       =
Some x

{% endhighlight %}

The constructors of `option` can be mimicked by the following utility functions

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

This doesn't quite work since F# records lack support for polymorphic fields. That is, each type variable must be listed on the left-hand side of its type definition. It would be silly to require option values to be parameterized over the return type of a function evaluating them. Instead, the return type needs to be [existentially quantified ](https://downloads.haskell.org/~ghc/5.00/docs/set/existential-quantification.html). Luckily all that is required is switching to standard interfaces:

{% highlight fsharp %}
type Option<'T> = abstract member Run : (unit -> 'U) ->  ('T -> 'U) ->  'U
{% endhighlight %}

and modifying the utility functions accordingly:

{% highlight fsharp %}
let run (o: Option<'T>) = o.Run

let none<'T> = 
  {new Option<'T> with member this.Run n s = n () }

let some (x: 'T) = 
  {new Option<'T> with member this.Run _ s = s x }

let map (f: 'T -> 'U) (op: Option<'T>) =  
  {new Option<'U> with member this.Run n s = op.Run n (f >> s) }
{% endhighlight %}

Finally here are a few examples of how to use the library:

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

On a practical note. Church-encoding is a form of [continuation passing style](http://en.wikipedia.org/wiki/Continuation-passing_style) and is sometimes used as an optimization technique. However, the F# encoding using interfaces is likely more expensive than sticking with regular algebraic data types.














