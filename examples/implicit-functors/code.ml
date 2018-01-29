(* Functors and modular implicits
 * opam switch 4.02.0+modular-implicits
 * eval `opam config env`
 *)
module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
let map {F : FUNCTOR} = F.map

implicit module ComposeFunctor {A : FUNCTOR} {B : FUNCTOR } = struct
  type 'a t = ('a B.t) A.t
  let map f = A.map (B.map f)
end

implicit module ProdFunctor {A : FUNCTOR} {B : FUNCTOR } = struct
  type 'a t = 'a A.t * 'a B.t
  let map f (x,y) = (A.map f x, B.map f y)
end

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

let e1 = map String.length ["list"; "of"; "strings"];;
let e2 = map sqrt (Some 9.);;

let e3 = map String.length [Some "foo"; None; Some "bar"];;
let e4 = map String.length (Some ["a"; "bc"; "def"]);;

let e4 = map String.length ([Some "apple"; None], Some "pear");;
