---
layout: post
title: Designing a library for printing recursive data structures
---
I've more than once run into situations in where I needed to pretty-print values from some recursive data structure - be it a prefix search tree, a language expression, XML or something else. Getting tired of having to implement the same type of functions over and again I decided to generalize the pattern. In this post I discuss the design of a tiny library for solving this particular problem. It's not a very challenging task in itself but provides a good opportunity to touch on a few different concepts in functional programming such as *deep* and *shallow embeddings*, [monoids] and *equational reasoning*.

## The Problem
To give a motivating example, consider the following type for representing XML data:

{% highlight fsharp %}
/// Attribute
type Attribute = string * string

/// XML is either a text-node or an element with attributes and children.
type XML =
    | Text of string
    | Node of string * list<Attribute> * list<XML>
{% endhighlight %}

An `Attribute` is key-value pair and an `XML` node is either some text or a node with attributes and a list of children.

Given a value of this type, we wish to rendere it as a nested set of blocks with proper indentation levels.

It's clear that we need some kind of abstraction so let's start by giving it a name, I call it `Printer`.  At the bare minimum we also need a function for evaluating a printer by turning it into a string:

{% highlight fsharp %}
/// Executes a printer, producing a string value.
val run : Printer -> string
{% endhighlight %}

Now, let's think about how to produce printers. We should naturally provide a function for lifting simple string values into printers:

{% highlight fsharp %}
/// Produces a printer that prints a string.
val print : string -> Printer
{% endhighlight %}

The interesting part is the ability to achieve nesting or indentation. The most intuitive way to express this is via a function that operates on a printer by indenting it one level:

{% highlight fsharp %}
// Indents a printer one level.
val indent : Printer -> Printer
{% endhighlight %}

Finally we need some means to compose printers; Let's require that our printer type forms a [monoid] by introducing an empty printer and a binary operator for combining two printers:

{% highlight fsharp %}
/// Doesn't print anything.
val empty : Printer

/// Combine two printers.
val add : Printer -> Printer -> Printer
{% endhighlight %}

You may wonder what good `empty` brings. One benefit is getting *sequencing* for free, that is the ability to combine a list of printers:

{% highlight fsharp %}
/// Sequences a list of printers.
let sequence : seq<Printer> -> Printer = Seq.fold add empty
{% endhighlight %}

Using the interface above, it's straight forward to print nested documents. Here is an example for manually outputting some HTML:

{% highlight fsharp %}
// Define a custom printer.
let htmlPrinter =
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
printfn "%s" (run htmlPrinter)   
{% endhighlight %}

produces output similar to:

{% highlight xml %}
<html>
  <body>
    Printed
  </body>
</html>
{% endhighlight %}

Here is the complete set of combinators defining the `API`:

{% highlight fsharp %}
type Printer

val run : Printer -> string

val print : string -> Printer

val indent : Printer -> Printer

val empty : Printer

val add : Printer -> Printer -> Printer
{% endhighlight %}

To ensure that the semantics is intuitive, there are a number of constraints that need to be satisfied by any particular implementation. I've identified the following ones:

1. `print >> run = id`
2. `forall p: add p empty = p`
3. `forall p: add empty p = p`
4. `forall p1,p2,p3: add p1 (add p2 p3) = add (add p1 p2) p3`
5. `forall p1,p2,p3: indent (add p1 p2) = add (indent p1) (indent p2)`

(1) states that (`run` is the inverse of `print`), i.e. printing a string and then running it gives back the same string. (2) and (3) means that `empty` must be left and right identtity for `add` and required in order for printer to form a [monoid]. (4) is also are part of the [monoid] constraints and implies that `add` is associative. (5) states that `indent` is [distributive](http://en.wikipedia.org/wiki/Distributive_property) over `add` (and also resembles the [functor] laws). It's required for safely being able to refactor expressions.

Guarenteeing (2), (3) and (4) is nececary in order to be able to implement have intuitive semantics for `sequence`. For instancee by ensuring that the following two printers are identical:

{% highlight fsharp %}
let pc1 = sequence [ p1; p2; p3 ]
let pc2 = sequence [p1 ; sequence [p2; p3]]
{% endhighlight %}

## A shallow embedding
To complete the library we now need to find a definition of the type `Printer`that allows for a feasible implementations of the required functions.

Thinking of the library as a small *Embedded Domain Specific Language* (EDSL), there are broadly speaking two implementation strategies available - *Deep* and *shallow* embeddings. Deep embeddings preserve the expression structure of the operations; This generally enables more optimization capabilities and also makes it possible to provide multiple *interpreters*. In a shallow embedding no intermediate data structure are used for building up expression trees, instead the semantics of an operation is part of its definition. 

Let's start by the simplest possible solution, not worrying about weather to support multiple interpreters or not. Applying the principal of [Denotational Design](http://conal.net/papers/type-class-morphisms/type-class-morphisms-long.pdf), we need to precisely define what it *means* to be a `Printer`. A printer is something that has the ability to print a nested structure. Parameterizing over the choice of how to print a line given an indentation level, this can be represented by the following function:

{% highlight fsharp %}
    type Printer = (int -> string -> string) -> string
{% endhighlight %}

In other words, a printer is a *black-box* that when applied to a function from an indentation level and a string to string, returns a pretty-printed structure. All the information of what to output is captured within the closure of the function. The fact that it's opaque, i.e. it is not possible to peek inside a printer to find out how it was constructed, places it in the category of *shallow embeddings*.

Let's see if the type is sufficient for implementing the interface. Starting with `run`:

{% highlight fsharp %}
let run (p: Printer) =
    p <| fun n s ->
        let space = String.Join("", List.replicate (n * 2) " ")
        sprintf "%s%s\n" space s
{% endhighlight %}

`Run` simply invokes the printer with a function that indents each line with two spaces per indentation level.

Lifting a string into a printer is also straight forward:

{% highlight fsharp %}
let print s = fun ind -> ind 0 s
{% endhighlight %}

The function turns a string into a printer that invokes the indentation argument with level 0.

To implement `indent` we need to transform a printer into a new one that adds one to the indentation level when applying the indent function:

{% highlight fsharp %}
let indent p = fun ind -> p (fun n -> ind (n + 1))
{% endhighlight %}

When it comes to `empty` we're left with little choice but to output an empty string:

{% highlight fsharp %}
let empty = fun _ -> ""
{% endhighlight %}

Last one is `add`:

{% highlight fsharp %}
let add p1 p2 = fun ind -> p1 ind + p2 ind
{% endhighlight %}
Which simply runs both printers and concatenates their output.

We also need to ensure that the implementations is compatible with the constraints regarding the semantics. 

First,  `(print >> run)` must be equivalent with the identity function:

{% highlight fsharp %}
(print >> run) s                                                    =
// Definition of function composition:
run (print s)                                                       =
// Definition of print:
run (fun ind -> ind 0 s)                                            =
// Definition of run:
(fun ind -> ind 0 s) (fun n s -> sprintf "%s%s" (space n) s)        =
// Apply the arguments (beta reduction):
sprintf "%s%s" (space 0) s)                                         =
// Definition of space and sprintf:
s
{% endhighlight %}

We also need to check that a printer fulfills the [monoid] constraints for `add`
and `empty`. Here is is the proof for left identity (2):

{% highlight fsharp %}
add p empty =
// Definition of add:
fun ind -> p ind + empty ind                                        =
// Definition of empty:
fun ind -> p ind + ((fun _ -> "") ind)                              =
// Beta reduction
fun ind -> p ind + ""                                               =
// Empty string is identity of string concatenation:
fun ind -> p ind                                                    =
p
{% endhighlight %}
In fact, these properties follow from the [monoid] properties of *string*. The proof in the other direction is symmetric. We also need to show that `add` is associative:

{% highlight fsharp %}
add p1 (add p2 p3)                                                  =
// Definition of add on the outer argument:
fun ind -> p1 ind + ((add p2 p3) ind)                               =
// Definition of add on inner argument:
fun ind -> p1 ind + ((fun ind -> p2 ind + p3 ind)   ind)            =
// Beta reduction:
fun ind -> p1 ind + p2 ind + p3 ind                                 =
// Associativity of string concatenation:
fun ind -> (p1 ind + p2 ind) + p3 ind                               =
/// Definition of add:
add (add p1 p2) p3
{% endhighlight %}

Again, the proof relies on the associativity of string concatenation.

Finally we need to show that `indent` is distributive according to (5). This  follows directly from the definitions of the two functions:

{% highlight fsharp %}
indent (add p1 p2)                                                  =
// Definition of indent:
fun ind -> (add p1 p2) (fun n -> ind (n + 1))                       =
// Definition of add
fun ind -> (fun ind -> p1 ind + p2 ind) (fun n -> ind (n + 1))      =
// Beta reduction:
fun ind -> (p1 (fun n -> ind (n + 1)) + p2 (fun n -> ind (n + 1)))  =
// Definition of indent:
add (indent p1) (indent p2)
{% endhighlight %}

To wrap it up, below is the complete listing of the implementation. I made the definition of `Printer` private, added a function for running printers with a custom indentation parameter and included operator aliases for `sequence` `print`, and `add`:

{% highlight fsharp %}
open System

/// A printer is a function from an indentation level to a list of strings.
type Printer = private { Run : (int -> string -> string) -> string}

/// Creates a printer.
let private mkPrinter f = {Run = f}

/// Creates a string of whitespace of the given length.
let private space n = String.Join("", List.replicate (n * 2) " ")

/// Create a printer from  a string.
let print s = mkPrinter <| fun ind -> ind 0 s

/// Indents a printer.
let indent p = mkPrinter <| fun ind -> p.Run ((+) 1 >> ind)

/// Runs a printer returning a string.
let runWith ind p = p.Run ind

/// Runs a printer returning a string.
let run = runWith <| fun n -> sprintf "%s%s\n" (space n)

/// An empty printer.
let empty = mkPrinter <| fun _ -> ""

/// Adds two printers.
let add tp1 tp2 = mkPrinter <| fun ind ->
     tp1.Run ind + tp2.Run ind

/// Concatenates a sequence of printers.
let sequence ps = Seq.fold add empty ps

/// Short for sequence.
let (!<) = sequence

/// Short for add.
let (<+>) = add

/// Short for print.
let (!) = print
{% endhighlight %}

Returning to the motivational example of printing XML, here is a complete implementation of a show function for the `XML` type:

{% highlight fsharp %}
/// Attribute
type Attribute = string * string

/// XML is either a text-node or an element with attributes and children.
type XML =
    | Text of string
    | Node of string * list<Attribute> * list<XML>

/// Pretty-prints an xml value.
let show =
    let showAttrs attrs = 
        let showAttr (n,v) = sprintf " %s=%s" n v
        String.Join("", List.map showAttr attrs)

    let rec show = function
        | Text t                -> 
            !t
        | Node (name,atrs,chs)  ->
            !<[
                !(sprintf "<%s%s>" name (showAttrs atrs))
                indent !<(List.map show chs)
                !(sprintf "</%s>" name)
            ]
    show >> run
{% endhighlight %}

Hopefully the example is straight forward to follow. In case you don't like the prefix operators, you could change the definition to use `sequence` instead of `!<`, `print` instead of `!`.

## A deep embedding
A deep embedding must preserve the structure of how a printer is assembled. This is required whenever you need to support multiple back-ends or different ways of interpreting expressions. Creating a data type for a deep embedding is straight forward, we basically just need to list the distinct language constructs. The following type will do:

{% highlight fsharp %}
// Deep embedding of printer type.
type Printer =
    | Empty
    | Print of string
    | Indent of Printer
    | Add of Printer * Printer
{% endhighlight %}

In this way, all operations are trivial. Here are the functions mirroring the constructors:
    
{% highlight fsharp %}
// Empty printer.
let empty = Empty

// Print a string.
let print = Print

// Indent a printer.
let indent p = Indent p

// Composing two printers.
let add p1 p2 =
    match p1,p2 with
    | Empty, p
    | p, Empty  -> p
    | _         -> Add(p1,p2)
{% endhighlight %}

The only interesting part is `add` which contains an optimization step for implementing left and right identity for `empty` in accordance with the specified semantics. All the work of evaluating a printer is pushed to the interpretors, in our case a function for constructing a string. Here are the the equivalent `runWith` and `run` functions:

{% highlight fsharp %}
// Executes a printer.
let runWith ind p =
    let sb = new Text.StringBuilder()
    let rec go n = function
        | Empty         -> ()
        | Print l       -> ignore <| sb.AppendLine (ind n l)
        | Indent p      -> go (n+1) p
        | Add (p1,p2)   -> go n p1 ; go n p2
    go 0 p
    sb.ToString()

/// Runs a printer returning a string.
let run = runWith <| fun n -> sprintf "%s%s\n" (space n)
{% endhighlight %}

Each language construct is handled separately with `Indent` and `Add` traversing their arguments recursively. A `Text.StringBuilder` object is used to accumulate the output of printed lines in order to improve on efficiency.

What about the semantics, how do we prove that the definition is compatible with the constraints listed above? What we really need to check is the validity of expressions with respect to a particular interpreter (in this case `run`). For instance looking at constraint (4) concerning associativity of `add`: `(add p1 (add p2 p3) = add (add p1 p2) p3)`, we're not interested in whether these expressions are identical or not; Only that they produce the same output for a given interpreter. However, whenever we are able to show that two expression are in fact identical it naturally follows that all possible interpretations are identical.

Showing that constraints (1) and (4) holds is similar to the example above. For left and right identity (2,3) it's possible to leverage the definition of `add` canceling out `Empty` values, but only in case the type constructors are hidden in order to rule out the construction of values such as `(Add (Print "Hello"), Empty)`. This fact introduces a subtle problem; On the one hand exposing the printer type is necessary in order to allow different interpretors to be defined. On the other hand, providing access to the constructors removes the control over how values are constructed. One solution would be to expose the core definitions in a separate module.

Using equational reasoning is slightly more complicated given the imperative style of the `runWith` function. The complete proofs are left as an exercise. 

Another approach is to provide a mapping from the deep to the shallow embedding. Assuming a module `ShallowPrinter` containing the shallow implementation from above, here is a function for performing the translation along with `runWith` function:

{% highlight fsharp %}
module SP = ShallowPrinter

/// Transforms a deep printer to a shallow one.
let rec toShallow = function
    | Empty         -> SP.empty
    | Print p       -> SP.print p
    | Indent p      -> SP.indent (toShallow p)
    | Add (p1,p2)   -> SP.add (toShallow p1) (toShallow p2)

/// Reusing the interpretor from the shallow embedding.
let runWith ind = toShallow >> SP.runWith ind

/// Runs a printer returning a string.
let run =  runWith <| fun n -> sprintf "%s%s\n" (space n)
{% endhighlight %}

Now, all proofs concerning the shallow implementation can be safely reused in order to show that the constraints are fulfilled for this definition of run.
 
Here is the a complete listing of a stand-alone deep embedding:

{% highlight fsharp %}
open System

// Deep embedding of printer type.
type Printer =
    | Empty
    | Print of string
    | Indent of Printer
    | Add of Printer * Printer

/// Creates a string of whitespace of the given length.
let private space n = String.Join("", List.replicate (n * 2) " ")

// Empty printer.
let empty = Empty

// Print a string.
let print = Print

// Indent a printer.
let indent p = Indent p

// Composing two printers.
let add p1 p2 =
    match p1,p2 with
    | Empty, p
    | p, Empty  -> p
    | _         -> Add(p1,p2)

// Execute a printer.
let runWith ind p =
    let sb = new System.Text.StringBuilder()
    let rec go n = function
        | Empty         ->
            ()
        | Print l        -> 
            ignore <| sb.AppendLine (ind n l)
        | Indent p      ->
            go (n+1) p
        | Add (p1,p2)   ->
            go n p1
            go n p2
    go 0 p
    sb.ToString()

/// Runs a printer returning a string.
let run =  runWith <| fun n -> sprintf "%s%s\n" (space n)

/// Concatenates a sequence of printers.
let sequence ps = Seq.fold add empty ps

/// Short for sequence.
let (!<) = sequence

/// Short for add.
let (<+>) = add

/// Short for print.
let (!) = print
{% endhighlight %}

At last, in order to illustrate how it is possible to define alternative interpretors of printer expressions, consider the following example that shows a printer as F# code for constructing the expression itself:

{% highlight fsharp %}
let showFSharp : Printer -> string =
    let rec show = function
        | Empty         ->
            !"Empty"
        | Print s       ->
            !(sprintf "Print \"%s\"" s)
        | Indent p      ->
            !<[
                !"Indent ("
                indent (show p)
                !")"
            ]
        | Add (p1,p2)   ->
            !<[
                !"Add ("
                indent (show p1)
                indent !","
                indent (show p2)
                !")"
            ]
    show >> run
{% endhighlight %}

using this function on the initial `htmlPrinter`  example, we actually retrieve and equivalent F# expression for defining the printer:

{% highlight fsharp %}
// Define a custom printer.
let htmlPrinter =
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

printfn "%s" (showFSharp htmlPrinter)
{% endhighlight %}

Yielding the output:

{% highlight fsharp %}
Add (
  Add (
    Print "<html>"
    ,
    Indent (
      Add (
        Add (
          Print "<body>"
          ,
          Indent (
            Print "Printed"
          )
        )
        ,
        Print "</body>"
      )
    )
  )
  ,
  Print "</html>"
)
{% endhighlight %}

Which is in fact vaalid F#. This would not have been possible using the shallow embedding.

## Summary
In this post I've addressed the problem of designing a library for pretty-printing recursive data structures. The approach taken is general and starts by identifying a minimal set of operations needed and then for each operation define a set of constraints (or *laws*) that any implementation must obey.
In order support compositionality of printers the [monoid] pattern was used.

Finally, two different realizations of the library was given, one *shallow* and one *deep* embedding. By using equational reasoning we were able to show that the semantical constraints were satisfied.

[monoid]: http://en.wikipedia.org/wiki/Monoid
[monoids]: http://en.wikipedia.org/wiki/Monoid
[functor]: http://en.wikipedia.org/wiki/Functor















