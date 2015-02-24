---
layout: page
title: Design patterns for functional programming
---

# Session 1 - The Category Pattern

In this session we introduced functional programming and its foundation in mathematics.
We showed that functional programming is based on $$\lambda$$-calculus and also presented
*Category Theory* as a tools for reasoning about programs. In particular we were able to
show that functions and data types form a Category, using F# to exemplify.

Finally we introduced two ways of constructing types, *Products* and *Co-products*, and their
categorical description.

Here are the note based on the slides. The last section contains a few exercises.

## Introduction

#### Why study functional programming?

* Fun
* Write expressive, modular, composable and elegant code.
* Reason about code and its composition (But, aren't these the buzzwords used to sell object orientation?).
* Forget low level *concurrency* primitives.
* Understand map-reduce.


#### OO vs. FP

If OO and FP both are claiming that they solve the same problems important to all software engineers, why neither of them does?

* There is no uniform accepted definition of object-orientation 
* The two might be orthogonal (Scala) and even cross-definable (Martin Odersky and Dotty)
* In contrast FP is ill defined, but there is a uniform theory. 

#### Ancient history

The theory of functional programming is a byproduct of the quest for the foundations of mathematics.

* 1910 - Russell and the theory of types.
* 1920 - Brouwer and the ideal of constructive mathematics.
* 1930 - Church and $$\lambda$$-calculus.
* 1947 - MacClane and Category theory.

#### Languages

* 1957, McCarthy LISP - using $$\lambda$$-calculus for actual programming.
* 1975, Milner, ML - adding types and type inference.
* Haskell - adding category theory (on user, library level).
* We will use F# as a vehicle for showing the principles of functional programming.


## Functions

#### Properties of functions 

We speak of functions in a mathematical sense, it is an unequivocal mapping.

* A function take argument(s) from its $$domain$$ and returns a value from its $$target (codomain)$$

* It always returns the same value for the same argument.

* There can be functions for any domain, so in particular functions can be chained, and the chained application of functions are still unequivocal ($$composition ( \circ )$$).

#### $$\lambda$$-calculus

- Variables:  $$x,y...\in Var \subset Term$$ 
- Abstraction: $$ x \in Var$$ and $$M \in Term$$ then $$\lambda x.M \in Term$$
- Application: $$ M \in Term$$ and $$N \in Term$$ then $$M N \in Term$$ 

Associated with the following set of rules:

* $$\alpha$$-reduction, renaming: $$ MxN \rightarrow^{\alpha} MyN $$
* $$\beta$$-reduction: $$\lambda x.y z \rightarrow^{\beta} y$$ $$\langle x/z \rangle$$
* $$\eta$$-reduction: $$\lambda x.Mx \rightarrow^{\eta} M $$ 


### Functions in F\#


{% highlight fsharp %}
// Constant
let answer = 42

// One argument function
let addTwo = fun x -> x + 2

// Equivalent
let addTwo' x = x + 2

// Function of two arguments
let concat fun s1 -> fun s2 -> s1 + " " + s2

// Equivalent
let concat' s1 s2 = s1 + " " + s2
{% endhighlight %}


In case no type annotations are given the F# compiler infers the type.
Definitions can also have explicity type annotations:

{% highlight fsharp %}

let answer : int = 42

let addTwo (x: int) = x + 2

let concat (s1: string) s2 : string = s1 + " " +  s2

{% endhighlight %}

#### Recursion
    

    
{% highlight fsharp %}
// Example
factorial 4 = 4 * 3 * 2 * 1
{% endhighlight %}    

{% highlight fsharp %}
// Recursion
let rec factorial n =
    if n <= 0 then 
        1
    else
        n * (factorial (n - 1))
{% endhighlight %}

Mutual recursion is defined by `let rec .. and`:
{% highlight fsharp %}
let rec isEven n = 
    if n < 0 then 
        isEven -n 
    elif n = 0 then
        true
    else 
        isOdd (n - 1)
and isOdd n = 
    if n = 0 then 
        false 
    else 
        isEven (n - 1)
{% endhighlight %}

#### Function composition

{% highlight fsharp %}
let isEven (n: int) : bool = n % 2 = 0

let length (s: string) : int = s.Length

let hasEvenLength = length >> isEven
{% endhighlight %}

{% highlight fsharp %}
let compose (f: 'B -> 'C) (g: 'A -> 'B) : 'A -> 'C = fun x -> f (g x)

let (<<) = compose

let (>>) g f = compose f g
{% endhighlight %}

{% highlight fsharp %}    
let isEven (n: int) : bool = n % 2 = 0

let length (s: string) : int = s.Length

let trim (s: string) : string = s.Trim()

let hasEvenNumChars = trim >> length >> isEven
{% endhighlight %}


#### Associativity
{% highlight fsharp %}
let hasEvenNumChars1 = trim >> (length >> isEven)
let hasEvenNumChars2 = (trim >> length) >> isEven
{% endhighlight %}

Is (>>) associative?
{% highlight fsharp %}
(f >> g) >> h                               =
// Definition of (>>)                       
(fun x -> g (f x)) >> h                     =
// Definition of (>>)                       
(fun y -> h ((fun x -> g (f x) y)           =
// Beta reduction                           
(fun y -> h (g (f y)))                      =

f >> (g >> h)                               =
// Definition of (>>)
fun x -> (g >> h) (f x)                     =
// Definition of (>>)
fun x -> (fun y -> h (g y)) (f x)           =
// Beta reduction
fun x -> h (g (f x))                        =
// Alpha conversion
fun y -> h (g (f y))                        =
{% endhighlight %}


## Categories
A category $$\mathcal{C}$$ is defined as:

1.  A set of objects, denoted $$Ob(\mathcal{C})$$

2. A set of arrows, $$Arr(\mathcal{C})$$ with functions  $$Dom,Codom  \in Arr(\mathcal{C}) \rightarrow Ob(\mathcal{C})$$


* Operation $$\circ \in Arr(C) \times Arr(C) \rightarrow Arr(C)$$, s.t. $$\forall f,g \in Arr(C)$$ with $$Codom(f) = Dom(g)$$, $$\exists g \circ f$$ and $$Dom(g \circ f) = Dom(f)$$ and $$Codom(g \circ f) = Codom(g)$$ 

* Associativivity: $$(g \circ f) \circ h = g \circ (f \circ h)$$

* $$\forall A \in Ob(\mathcal{C})  \exists id_A \in Arr(\mathcal{C})$$ s.t. $$Dom(id_A) = Codom(id_A) = A$$

* Identity: if $$f : A \longrightarrow_{f} B$$ then $$id_B \circ f = f \circ id_A = f$$

![Category](img/category.png)

#### The Category of data and functions
{% highlight fsharp %}
// Composition operator
let (<<) f g = fun x -> f (g (x))

// Identity
let id (x: 'T) : 'T = x
{% endhighlight %}

{% highlight fsharp %}
// Associativity
(f >> g) >> h = f >> (g >> h)

// Left identity
f >> id = f

// Right identity
id >> g = g
{% endhighlight %}


## Types

* There is a problem. You can't really take simple untyped $$\lambda$$-calculi as a good guide for calculations.

* Consider the following term: $$ \lambda x.(x x)  \lambda x.(x x)$$.

* To defeat this Church and Curry introduced types to $$\lambda$$-calculi and made it a firm foundation for computation.


- Types are essential to maintain consistency in a formal
    system. In $$\lambda$$-calculi type-able terms are
    terminating calculations (normalisation).

- Types are in direct correspondence with logic, different type
    systems with different logical calculi.

- Types are not labels on memory cells, they are not merely
    assuring absence of segmentation faults, but are essential tools
    within the compilers to check correctness. 

### Types in F\#
Built in:
{% highlight fsharp %}
int, bool, char, unit, float, string
{% endhighlight %}

#### Products:
{% highlight fsharp %}
// Tuples
let myTuple : (int * string * bool) = (42, "The answer", true)
{% endhighlight %}

{% highlight fsharp %}
// Records
type Person = 
    {
        Name : string
        Email : string
        Age : int
    }

// Record construction.
let gabor = {Name = "Gabor"; Email = "gabor@foobar.com"; Age = 45}

// Access properties.
let gaborEmail = gabor.Email

// Record modification.
let olderGabor = {gabor with Age = 46}
{% endhighlight %}


#### Sum types (Co-products)
{% highlight fsharp %}
type Dice = | One | Two | Three | Four | Five | Six
    
let randomDice = Five
{% endhighlight %}


{% highlight fsharp %}

/// Union type with arguments.
type Contact =
    | Email of string
    | Phone of string
    | Address of Address

let contact = Email "gabor@foobar.com"
{% endhighlight %}

{% highlight fsharp %}

// Parameterized
type Option<'T> =
    | Some of 'T
    | None

let someInt : Option<int> = Some 42

let nothing : Option<string> = None
{% endhighlight %}

Product types in category theory
	
![Category](img/product.png)

## Pattern matching
{% highlight fsharp %}   
type Address = {Street : string; Number : int; Zip : string}

type Contact =
    | Email of string
    | Phone of string
    | Address of Address

let printContact (contact: Contact) =
    match contact with
    | Email e    -> printfn "Email: %e" s
    | Phone f    -> printfn "Phone: %s" f
    | Address ad -> printfn "Adress: %s %A %s" ad.Street ad.Number ad.Zip

{% endhighlight %}

## Recursive data structures
Example - representing boolean expressions
{% highlight fsharp %}
type Exp =
    | True
    | False
    | Not of Exp
    | And of Exp * Exp
    | Or of Exp * Exp

let example = And (Not (Or (False,True)), True)
{% endhighlight %}
{% highlight fsharp %}

let rec eval exp =
    match exp with
    | True          -> true
    | False         -> false
    | Not e         -> not (eval e)
    | And (e1,e2)   -> (eval e1) && (eval e2)
    | Or (e1,e2)    -> (eval e1) || (eval e2)
{% endhighlight %}


Example - Representing natural numbers using sum types:
{% highlight fsharp %}
type Nat =
    | Zero
    | Succ of Nat

let rec add n m =
    match n, m with
    | Zero, m       -> m
    | n, Zero       -> n
    | Succ n', _    -> Succ(add n' m)

let (<+>) = add

let one = Succ Zero
let two = one <+> one
let three = one <+> two
let five = two <+> three

// Exercise
let toInt (n: Nat) : int = ??
{% endhighlight %}

Representing lists:
{% highlight fsharp %}
type List<'T> =
    | Nil
    | Cons of ('T * List<'T>)

[] = Nil
(::) x xs = Cons(x,xs)
[a,b,c,d] = a :: b :: c :: d :: []
{% endhighlight %}

Constructing lists
{% highlight fsharp %}
let primes = [1; 2; 3; 5; 7; 11; 13]

let persons = [
    {Name = "Gabor"; Email = "gabor@foobar.com"; Age = 59}
    {Name = "Sally"; Email = "sally@foobar.com"; Age = 25}
    {Name = "Attila"; Email = "attila@foobar.com"; Age = 31}
]
{% endhighlight %}

Pattern matching lists:
{% highlight fsharp %}

let isEmpty xs =
    match xs with
    | []        -> true
    | _         -> false

let rec containsGabor (ps: list<Person>) : bool =
    match ps with
    | []        -> false
    | p :: ps   -> p.Name = "Gabor" || containsGabor ps
{% endhighlight %}

  
## Exercises

#### Natural numbers
Given the definition of `Nat` for representing natural numbers,
define two functions `toNat` and `fromNat` that proves that positive
integers are isomorphic `Nat`.

{% highlight fsharp %}
let fromNat (n: Nat) : int = ??
let toNat (n: int) : Nat = ??
{% endhighlight %} 

Also who that for any positive integer `n` the following hold:

{% highlight fsharp %}
(toNat >> fromNat) n = n
{% endhighlight %}

and

{% highlight fsharp %}
(fromNat >> toNat) = id
{% endhighlight %}


#### Boolean expression
Extend the definition of boolean expression to also include variables
with a name. Modify the eval function to accept an extra argument for
looking up the value of a variable:

{% highlight fsharp %}
let eval (env: String -> bool) (exp: Exp) : bool = ??

{% endhighlight %}



