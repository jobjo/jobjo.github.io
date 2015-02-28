---
layout: post
title: Church encoding and existentially quantified types in F#
---

Can you imagine an F# compiler that doesn't understand discriminated unions (aka algebraic data types)? 
For sure not  an attractive scenario but perhaps just not as horrifying as you might expect. 
For example consider how the familiar option type is defined in F#:

{% highlight fsharp %}
type option<'T> =
  | Some of 'T
  | None
{% endhighlight %}

Along with the ability to pattern match over `option` values:

{% highlight fsharp %}
match someValue with
| Some x  -> ...
| None    -> ...
{% endhighlight %}

Is it possible to construct an isomorphic data type without using discriminated unions?

One solution is given by [church encodings](http://en.wikipedia.org/wiki/Church_encoding); An algebraic data type is encoded as a 
function accepting one continuation per constructor. Here is a concrete example of such a function:

{% highlight fsharp %}
val optionValue<'T> : (unit -> 'T) -> (int -> 'T) -> 'T
{% endhighlight %}

What are its possible implementations? There are only two sensible things it may do; Either invoke the first argument
with a unit value or apply the second argument to some already fixed integer. It is straight forward to find out which case applies by converting it to a regular `option<int>`.

{% highlight fsharp %}
let result : option<int> = optionValue (fun _ -> None) Some
{% endhighlight %}

Relaxing the constraint of using `int`, the generalized type `(unit -> 'T) -> ('U -> 'T) -> 'T` can be used to represent 
optional values of arbitrary type. The following definitions show that it's possible to translate back and forth between the regular option type and its church encoded counterpart.

{% highlight fsharp %}
// Church-encode an option value.
let fromOption = function
    | Some x    -> fun _ s -> s x
    | None      -> fun n _ -> n ()

// Decode a church encoded option value.
let toOption op = op (fun _ -> None) Some
{% endhighlight %}

The constructors of `option` can be mimicked by the following utility functions:

{% highlight fsharp %}
// Constructs an empty value.
let none = fun n f -> n ()

// Constructs some value
let some x = fun _ f -> f x
{% endhighlight %}

As well as defining a function for mapping over optional values:

{% highlight fsharp %}
// Map over an option value.
let map f op = fun n s -> op n (fun x -> s (f x))

// Example - Evaluates to Some 42
toOption <| map ((+) 1) (some 42)

// Evaluates to None
toOption <| map ((+) 1) none
{% endhighlight %}

In order to ensure that the church option type is isomorphic to the regular version, 
one also needs to show that `(fromOption >> toOption = id)` and symmetrically that `(toOption >> fromOption = id)`.
Simple equational reasoning does the trick. Both the cases *some* and *none* must be considered. 
First, going from a standard option value to a church encoded value and back should yield the same result:

{% highlight fsharp %}

// Identity for None
(fromOption >> toOption) None                      =
// Definition of (>>)
toOption (fromOption None)                         =
// Definition of fromOption
toOption (fun n _ -> n ())                         =
// Definition of toOption
(fun n _ -> n ()) (fun _ -> None) Some             =
// Beta reduction
None

// Identity for Some x
(fromOption >> toOption) (Some x)                  =
// Definition of (>>)
toOption (fromOption (Some x))                     =
// Definition of fromOption
toOption (fun _ s -> s x)                          =
// Definition of toOption
(fun _ s -> s x) (fun _ -> None) Some              =
// Beta reduction
Some x

{% endhighlight %}

And the other way around: 

{% highlight fsharp %}

Identity for none
(toOption >> fromOption) none                      =
// Defintion of none      
(toOption >> fromOption) (fun n _ -> n ())         =
// Definition of >>
fromOption (toOption (fun n _ -> n ()))            =
// Defintion of toOption                           =
fromOption ((fun n _ -> n ()) (fun _ -> None) Some =
// Beta reduction
fromOption None
// Definition of fromOption
fun n _ -> n ()                                    =
none

Identity for (some x)
(toOption >> fromOption) (some x)                  =
// Definition of some x
(toOption >> fromOption) fun _ f -> f x            =
// Definition of >>
fromOption (toOption (fun _ f -> f x))             =
// Definition of toOption
fromOption ((fun _ f -> f x) (fun _ -> None) Some) =
// Beta reduction
fromOption (Some x)                                =
fun _ f -> f x                                     =
// Definition of some
some x

{% endhighlight %}


In order to package it up as a library it would be nice to create a type synonym for the church encoded option type. How about simply wrapping the function type inside a record?

{% highlight fsharp %}
type Option<'T> = internal {Run<'U> : ((unit -> 'U) -> ('T -> 'U) -> 'U)}
{% endhighlight %}

This doesn't quite work since F# records lack support for polymorphic properties. That is, each type variable must be listed on the left-hand side of its type definition. It would be silly to require option values to be parameterized over the return type of a function evaluating them. Instead, the return type needs to be [existentially quantified ](https://downloads.haskell.org/~ghc/5.00/docs/set/existential-quantification.html). Luckily all that is required is switching to standard interfaces:

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

Looking at  `printResult`, the similarities with ordinary pattern matching over algebraic data types is striking.

On a practical note, church encoding is a form of [continuation passing style](http://en.wikipedia.org/wiki/Continuation-passing_style) 
and is sometimes used as an optimization technique. However, the F# encoding using interfaces is likely more expensive than sticking with regular algebraic data types.














