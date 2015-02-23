---
layout: page
title: Design patterns for functinal programming
---

## Why study functional programming

* Fun
* Write expressive, modular, composable and elgegant code.
* Reason about code and its composition (But, aren't these the buzzwords used to sell object orientation?).
* Forget low level *concurrency* primitives
* Understand map-reduce

### Introduction


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

*******************************************************************************

### Introduction

#### Languages

* 1957, McCarthy LISP - using $$\lambda$$-calculus for actual programming.
* 1975, Milner, ML - adding types and type inference.
* Haskell - adding category theory (on user, library level).
* We will use F# as a vehicle for showing the principles of functional programming.


### Functions

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


### Functions in F#


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


#### Explicitly typed parameters

{% highlight fsharp %}

let answer : int = 42

let addTwo (x: int) = x + 2

let concat (s1: string) s2 : string = s1 + " " +  s2

{% endhighlight   %}

#### Recursion
    
{% highlight fsharp %}

// Recursion
let factorial (n: int) : int = ???

// Example
factorial 4 = 4 * 3 * 2 * 1

{% endhighlight %}


#### Recursion

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

#### Explicitly typed parameters

{% highlight fsharp %}

let answer : int = 42

let addTwo (x: int) = x + 2

let concat (s1: string) s2 : string = s1 + " " +  s2
{% endhighlight %}

#### Recursion
    
    // Recursion
    let factorial (n: int) : int = ???

    // Example
    factorial 4 = 4 * 3 * 2 * 1
    

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

*******************************************************************************
### Functions in F#
#### Explicitly typed parameters

    let answer : int = 42

    let addTwo (x: int) = x + 2

    let concat (s1: string) s2 : string = s1 + " " +  s2

*******************************************************************************
### Functions in F#
#### Recursion
    
    // Recursion
    let factorial (n: int) : int = ???

    // Example
    factorial 4 = 4 * 3 * 2 * 1
*******************************************************************************
### Functions in F#
#### Recursion
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

*******************************************************************************
### Functions in F#
#### Explicitly typed parameters

    let answer : int = 42

    let addTwo (x: int) = x + 2

    let concat (s1: string) s2 : string = s1 + " " +  s2

*******************************************************************************
### Functions in F#
#### Recursion
    
    // Recursion
    let factorial (n: int) : int = ???

    // Example
    factorial 4 = 4 * 3 * 2 * 1
*******************************************************************************
### Functions in F#
#### Recursion
    
    // Recursion
    let rec factorial n =
        if n <= 0 then 
            1
        else
            n * (factorial (n - 1))

*******************************************************************************

### Functions in F#
#### Recursion

    // Mutual recursion
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

*******************************************************************************
### Function composition

    let isEven (n: int) : bool = n % 2 = 0

    let length (s: string) : int = s.Length

    let hasEvenLength = ???


*******************************************************************************
### Function composition

    let isEven (n: int) : bool = n % 2 = 0

    let length (s: string) : int = s.Length

    let hasEvenLength s = isEven (length s)

*******************************************************************************
### Function composition

    let isEven (n: int) : bool = n % 2 = 0

    let length (s: string) : int = s.Length

    let hasEvenLength = length >> isEven


*******************************************************************************
### Function composition

    let compose (f: 'B -> 'C) (g: 'A -> 'B) : 'A -> 'C = fun x -> f (g x)
    
    let (<<) = compose
    
    let (>>) g f = compose f g


*******************************************************************************
### Function composition
    
    let isEven (n: int) : bool = n % 2 = 0

    let length (s: string) : int = s.Length

    let trim (s: string) : string = s.Trim()

    let hasEvenNumChars = trim >> length >> isEven

*******************************************************************************
### Function composition
#### Associativity
    let hasEvenNumChars1 = trim >> (length >> isEven)
    let hasEvenNumChars2 = (trim >> length) >> isEven


*******************************************************************************
### Function composition
#### Is (>>) associative?

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
    fun x -> (fun y -> h (g y)) (f x)  
    // Beta reduction
    fun x -> h (g (f x))
    // Alpha conversion
    fun y -> h (g (f y))

*******************************************************************************
### Categories
A category $$\mathcal{C}$$ is defined as:

1.  A set of objects, denoted $$Ob(\mathcal{C})$$

2. A set of arrows, $$Arr(\mathcal{C})$$ with functions  $$Dom,Codom  \in Arr(\mathcal{C}) \rightarrow Ob(\mathcal{C})$$


*******************************************************************************
### Categories

* Operation $$\circ \in Arr(C) \times Arr(C) \rightarrow Arr(C)$$, s.t. $$\forall f,g \in Arr(C)$$ with $$Codom(f) = Dom(g)$$, $$\exists g \circ f$$ and $$Dom(g \circ f) = Dom(f)$$ and $$Codom(g \circ f) = Codom(g)$$ 

* Associativivity: $$(g \circ f) \circ h = g \circ (f \circ h)$$

* $$\forall A \in Ob(\mathcal{C})  \exists id_A \in Arr(\mathcal{C})$$ s.t. $$Dom(id_A) = Codom(id_A) = A$$

* Identity: if $$f : A \longrightarrow_{f} B$$ then $$id_B \circ f = f \circ id_A = f$$

*******************************************************************************
### Categories
![Category](images/category.png)


*******************************************************************************
### The Category of data and functions

    // Composition operator
    let (<<) f g = fun x -> f (g (x))

    // Identity
    let id (x: 'T) : 'T = x

*******************************************************************************
### The Category of data and functions

    // Associativity
    (f >> g) >> h = f >> (g >> h)

    // Left identity
    f >> id = f

    // Right identity
    id >> g = g


*******************************************************************************
### Types
#### Types for functions

* There is a problem. You can't really take simple untyped $$\lambda$$-calculi as a good guide for calculations.

* Consider the following term: $$ \lambda x.(x x)  \lambda x.(x x)$$.

* To defeat this Church and Curry introduced types to $$\lambda$$-calculi and made it a firm foundation for computation.

*******************************************************************************

### Types
#### Remarks
- Types are essential to maintain consistency in a formal
    system. In $$\lambda$$-calculi type-able terms are
    terminating calculations (normalisation).

- Types are in direct correspondence with logic, different type
    systems with different logical calculi.

- Types are not labels on memory cells, they are not merely
    assuring absence of segmentation faults, but are essential tools
    within the compilers to check correctness. 

*******************************************************************************
### Types in F#
#### Built in

    int, bool, char, unit, float, string

*******************************************************************************
### Types in F#
#### Products
    // Tuples
    let myTuple : (int * string * bool) = (42, "The answer", true)

*******************************************************************************
### Types in F#
#### Products
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


*******************************************************************************
### Types
#### Sum types (Co-products)

    type Dice = | One | Two | Three | Four | Five | Six
    
    let randomDice = Five


*******************************************************************************
### Types
#### Sum types (Co-products)

    /// Union type with arguments.
    type Contact =
        | Email of string
        | Phone of string
        | Address of Address
    
    let contact = Email "gabor@foobar.com"

*******************************************************************************
### Types
#### Sum types (Co-products)

    // Parameterized
    type Option<'T> =
        | Some of 'T
        | None

    let someInt : Option<int> = Some 42

    let nothing : Option<string> = None


*******************************************************************************
### Types
#### Product types in category theory
	
![Category](images/product.png)


*******************************************************************************

### Pattern matching
   
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


*******************************************************************************
### Recursive data structures
#### Example - representing boolean expressions

    type Exp =
        | True
        | False
        | Not of Exp
        | And of Exp * Exp
        | Or of Exp * Exp

    let example = And (Not (Or (False,True)), True)


*******************************************************************************
### Recursive data structures
#### Example - representing boolean expressions
    type Exp =
        | True
        | False
        | Not of Exp
        | And of Exp * Exp
        | Or of Exp * Exp

    let rec eval exp =
        match exp with
        | True          -> true
        | False         -> false
        | Not e         -> not (eval e)
        | And (e1,e2)   -> (eval e1) && (eval e2)
        | Or (e1,e2)    -> (eval e1) || (eval e2)


*******************************************************************************
### Recursive data structures
#### Representing natural numbers
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


*******************************************************************************
### Recursive data structures
#### Lists
    type List<'T> =
        | Nil
        | Cons of ('T * List<'T>)
    
    [] = Nil
    (::) x xs = Cons(x,xs)
    [a,b,c,d] = a :: b :: c :: d :: []


*******************************************************************************
### Lists
#### Constructing lists
    let primes = [1; 2; 3; 5; 7; 11; 13]

    let persons = [
        {Name = "Gabor"; Email = "gabor@foobar.com"; Age = 59}
        {Name = "Sally"; Email = "sally@foobar.com"; Age = 25}
        {Name = "Attila"; Email = "attila@foobar.com"; Age = 31}
    ]

*******************************************************************************
### Lists
#### Pattern matching

    let isEmpty xs =
        match xs with
        | []        -> true
        | _         -> false

    let rec containsGabor (ps: list<Person>) : bool =
        match ps with
        | []        -> false
        | p :: ps   -> p.Name = "Gabor" || containsGabor ps

*******************************************************************************
### Lists
#### Aggregating elements

    let rec sum (xs: list<int>) : int = ??

*******************************************************************************
### Lists
### Aggregating elements

    let rec sum (xs: list<int>) : int =
        match xs with
        | []        -> 0
        | x :: xs   -> x + sum xs

*******************************************************************************
### Lists
#### Aggregating elements
    let rec concatenate (xs: list<string>) : string =
        match xs with
        | []        -> ""
        | x :: xs   -> x  +  concatenate xs

*******************************************************************************
### Lists
#### Aggregating elements

    let rec allLengthThree (ss: list<string>) : bool = ??

*******************************************************************************
### Lists
#### Aggregating elements

    let rec allLengthThree (ss: list<string>) : bool =
        match ss with
        | []        -> true
        | s :: ss   -> s.Length = 3 && allLengthThree ss


*******************************************************************************
### Lists
#### Aggregating elements

    let rec foldRight (f: 'T -> 'S -> 'S) (z: 'S) (xs: list<'T>) : 'S =
        match xs with
        | []        -> z
        | x :: xs   -> f x (foldRight f z xs)

    let sum = foldLeft (+) 0

    let concatenate = foldLeft (+) ""

    let rec allLengthThree (ss: list<string>) : bool = ??


*******************************************************************************
### Lists
#### Mapping elements

    let rec allNames (ps : list<Person>) : list<Person> =
        match ps with
        | []        -> []
        | p :: ps   -> p.Name :: allNames ps

*******************************************************************************
### Lists
#### Mapping elements

    let rec squareAll (xs : list<int>) : list<int> =
        match xs with
        | []        -> []
        | x :: xs   -> x * x :: squareAll xs

    let rec allLengths (ss: list<string>) : list<int> =
        match ss with
        | []        -> []
        | s :: ss   -> s.Length :: allLengths ss    

*******************************************************************************
### Lists
#### Mapping elements

    let rec map (f: 'T -> 'S) (xs : list<'T>) : list<'S> = ??


*******************************************************************************
### Lists
#### Mapping elements

    let rec map (f: 'T -> 'S) (xs : list<'T>) : list<'S> =
        match xs with
        | []        -> []
        | x :: xs   -> f x :: map f xs

    let allNames = map (fun p -> p.Name)

    let squareAll = map (fun x -> x * x)

    let allLengths = map (fun s -> s.Length)

*******************************************************************************
### Lists
#### Mapping elements

    let rec map (f: 'T -> 'S) (xs : list<'T>) : list<'S> = 
        foldRight (fun x xs -> f x :: xs) [] xs

*******************************************************************************
### Lists
#### Filtering elements

    let allExceptGabor : list<Person> -> list<Person> = 
        filter (fun p -> p.Name <> "Gabor")

*******************************************************************************
### Lists
#### Filtering elements

    let allExceptGabor : list<Person> -> list<Person> = 
        filter (fun p -> p.Name <> "Gabor")

    let filter (pred: 'T -> bool) (xs: list<'T>) : list<'T> = ??


*******************************************************************************
### Lists
#### Filtering elements

    let filter (pred: 'T -> bool) (xs: list<'T>) : list<'T> = 
        foldRight (fun x xs -> if pred x then x :: xs else xs) [] xs


*******************************************************************************
### Lists
#### Collecting elements

    let allChars = flatMap (List.ofSeq) ["abc"; "def"]

    allChars ["abc"; "de" ; "f"] = ['a'; 'b'; 'c'; 'd'; 'e'; 'f']


*******************************************************************************
### Lists
####  Collecting elements

    let flatMap f xs = foldRight (fun x xs -> f x @ xs) [] xs
    


*******************************************************************************
### Lists
#### Composing functions over lists

    type Person = 
        { 
            Name : string
            Email : string
            Age : int 
        }

    let persons = 
        [
            {Name = "Gabor"; Email = "gabor@foobar.com"; Age = 59}
            {Name = "Sally"; Email = "sally@foobar.com"; Age = 25}
            {Name = "Attila"; Email = "attila@foobar.com"; Age = 31}
        ]

    let sumOfAllAgesOfPersonsNotNamedGabor =
        List.filter (fun p -> p.Name <> "Gabor")
        >> List.map (fun p -> p.Age)
        >> List.fold (+) 0

  
*******************************************************************************
### Summary

1. Functional programming orginiates from $$\lambda calculus$$.

2. Functions and function composition - Data types and functions form a Category!

3. Defining custom types (Products and Co-products).

4. Functions over lists - `fold`, `map`, `filter` and `flatMap`.


