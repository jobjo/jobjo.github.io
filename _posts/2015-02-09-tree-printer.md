---
layout: post
title: Designing a library for printing recursive data-structures
---
Doing functional programming, I've more than once ran into situations in where I needed to pretty print values from a recursive data structure - be it a prefix search tree, a language expression, XML data or something else.

Getting tired of having to implement the same type of functions for printing values (often for debugging purposes) I decided to generalize the pattern. In this post I discuss the design of a tiny library for solving this particular problem. This is not a hard problem but it provides a good opportunity to touch on some of the fundamental concepts of functional programming.


## The Problem
To give a motivating example - Consider a type representing XML data:

{% highlight fsharp %}
type Attribute = string * string
type XML =
    | Text of string
    | Node of list<Attribute> * list<XML>
{% endhighlight %}

An `Attribute` is key-value pair and an `XML` node is either some text or a node with attributes and a list of children.

Given a value of this type we'd like to be able to render it as a nested set of blocks with proper indentation levels. What would a library for such a thing look like? 

It's clear that we need some kind of abstraction so let's start by giving it a name. I call it `Printer`. What kind of operations are needed in order satisfy the initial requirements? It's clear that we need a function for turning a printer into a string:

{% highlight fsharp %}
/// Executes a printer, producing a string value.
val run : Printer -> string
{% endhighlight %}

Now, let's think about how to produce printers. We should naturally provide a function for lifting simple string values into printers:

{% highlight fsharp %}
/// Produces a printer that prints a string.
val print : string -> Printer
{% endhighlight %}

The interesting part is the ability to express nesting or indentation. The most intuitive way to express this is via a function that operates on a printer (indenting it one level):

{% highlight fsharp %}
// Indents a printer one level.
val indent : Printer -> Printer
{% endhighlight %}

Finally, compositionality is alwasy desirable - Let's require that our printer type forms a `Monoid` by introducing an empty printer and a binary operator for adding two printers:

{% highlight fsharp %}
/// Doesn't print anything.
val empty : Printer
/// Combine two printers.
val add : Printer -> Printer -> Printer
{% endhighlight %}


You may wonder why `empty` is needed. The reason is that we then get sequencing for free, that is the ability to combine a list of printers:

{% highlight fsharp %}
/// Sequences a list of printers.
let sequence ps = Seq.fold add empty ps
{% endhighlight %}


Using the interface above, it's straight forward to print nested documents. Here is an example for manually outputting some `XML`:

{% highlight fsharp %}
// Define a custom printer.
let xmlPrinter =
    sequence [
        print "<html>"
        indent <| 
            sequence [
                print "<body>"
                indent <| 
                    sequence [
                        print "Printed"
                    ]
                print "</body>"
            ]
        print "</html>"
    ]
{% endhighlight %}

With the expectation that running this printer:

{% highlight fsharp %}
printfn "%s" (run xmlPrinter)   
{% endhighlight %}

produces output looking simliar to:

{% highlight xml %}
<html>
  <body>
    Printed
  </body>
</html>
{% endhighlight %}

To summarize, here is the complete set of operations to be implemented in order to realize the library:

{% highlight fsharp %}
type Printer

val run : Printer -> string
val print : string -> Printer

val indent : Printer -> Printer
val empty : Printer
val add : Printer -> Printer -> Printer
{% endhighlight %}

To ensure that the semantics is intuitive, the following constraints must hold:

1. `print >> run = id`  (`run` is the inverse of `print`)
2. `add p empty = p` (`empty` is left identity)
3. `add empty p = p` (`empty` is right identity)
4. `forall p1,p2,p3: add p1 (add p2 p3) = add (add p1 p2) p3`  (`add` is associative)

(1) Is to state that printing a string and then running it gives back the same string. (2), (3) and (4) are part of the *monoid constraints* and important in order to be able to implement the `sequence` operator correctly.

## Implementation
To complete the library we now need to find a definition of the type `Printer`that allows for feasible implementations of the required functions.

According to principal of [Denotational Design](http://conal.net/papers/type-class-morphisms/type-class-morphisms-long.pdf) we should start by thinking of what it *means* to be a `Printer`. A printer is something that has the ability to print a nested structure. Parameterizing over the choice of how to print a line given an indentation level, this is captured by a function:

{% highlight fsharp %}
    type Printer = (int -> string -> string) -> string
{% endhighlight %}

In other words, a printer is a black-box that when given a function from an indentation level and a string to string, returns a pretty-printed structure. All the information of what to print is captured within the closure of the function. Let's see if the definition is sufficient for implementing the interface. Starting with `run`:

{% highlight fsharp %}
    let run (p: Printer) =
        p <| fun n s ->
            let space = String.Join("", List.replicate (n * 2) " ")
            sprintf "%s%s\n" space s
{% endhighlight %}

It simply invokes the printer with a function that indents each line with two spaces per indentation level.

Lifting a string into a printer is also straight forward:

{% highlight fsharp %}
let print s = fun indent -> indent 0 s
{% endhighlight %}
The function turns a string into a printer that invokes the indentation argument with level 0.

To implement `indent` we need to transform a printer into a new one that adds one to the indentation level when invoking the indent function:

{% highlight fsharp %}
let indent p = fun indent -> p (fun n -> indent (n + 1))
{% endhighlight %}

When it comes to `empty` we're left with little choice but to output an empty string:

{% highlight fsharp %}
let empty = fun _ -> ""
{% endhighlight %}

Last one is `add`:

{% highlight fsharp %}
let add p1 p2 = fun indent -> p1 indent + p2 indent
{% endhighlight %}
Which simply runs both printers and concatenates their output.

We also need to ensure that the implementations is compatible with the constraints regarding semantics. 

First,  `print >> run` should be the identity function:

{% highlight fsharp %}
(print >> run) s                                                    =
run (print s)                                                       =
run (fun indent -> indent 0 s)                                      =
(fun indent -> indent 0 s) (fun n s -> sprintf "%s%s" (space n) s)  =
sprintf "%s%s" (space 0) s)                                         =
s
{% endhighlight %}

We also need to check that a printer fulfills the monoid constraints for `add`
and `empty`. Here is is the proof for left identity (2):

{% highlight fsharp %}
add p empty =
fun indent -> p indent + empty indent                               =
fun indent -> p indent + ((fun _ -> "") indent)                     =
fun indent -> p indent + ""                                         =
fun indent -> p indent                                              =
p
{% endhighlight %}
In fact, these properties follow from the *monoid* properties of *string*. The proof is the other direction is symmetric. We also need to show that `add` is associative:

{% highlight fsharp %}
add p1 (add p2 p3)                                                  =
fun ind -> p1 ind + ((add p2 p3) ind)                               =
fun ind -> p1 ind + ((fun ind -> p2 ind + p3 ind)   ind)            =
fun ind -> p1 ind + p2 ind + p3 ind                                 =
fun ind -> (p1 ind + p2 ind) + p3 ind                               =
add (add p1 p2) p3
{% endhighlight %}

Again, the proof relies on the associativity of string concatenation.

To wrap it up, below is the complete listing of the implementation. I made the definition of `Printer` private, added a function for running printers with a custom indentation function. Finally added two operator aliases for `sequence` and `print`, and `add`:

{% highlight fsharp %}
open System

/// A printer is a function from an indentation level to a list of strings.
type Printer = private { Run : (int -> string -> string) -> string}

/// Creates a printer.
let private mkPrinter f = {Run = f}

/// Creates a string of whitespace of the given length.
let private space n = String.Join("", List.replicate (n * 2) " ")

/// Create a printer from  a string.
let print s = mkPrinter <| fun indent -> indent 0 s

/// Indents a printer.
let indent p = mkPrinter <| fun indent -> p.Run ((+) 1 >> indent)

/// Runs a printer returning a string.
let runWith indent p = p.Run indent

/// Runs a printer returning a string.
let run = runWith <| fun n -> sprintf "%s%s\n" (space n)

/// An empty printer.
let empty = mkPrinter <| fun _ -> ""

/// Adds two printers.
let add tp1 tp2 = mkPrinter <| fun indent ->
     tp1.Run indent + tp2.Run indent

/// Concatenates a sequence of printers.
let sequence ps = Seq.fold add empty ps

/// Short for sequence.
let (!<) = sequence

/// Short for add.
let (<+>) = add

/// Short for print.
let (!) = print
{% endhighlight %}

<!--
Thinking of the library as a small embedded domains specific language (EDSL), there are broadly speaking two implementation strategies - *Deep* versus *shallow* embedding. Deep embeddings use a data structure that preserves the expression structure of the operations; This generally enables more optimization capabilities and also makes it possible to provide multiple *interpreters*. In a shallow embedding no intermediate data structure is used for building up expression trees, instead the semantics of an operation is part of its definition.
-->




