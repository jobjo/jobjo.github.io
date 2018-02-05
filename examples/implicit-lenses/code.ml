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

type ('a, 'b) lens = {F : FUNCTOR} -> ('b -> 'b F.t) -> 'a -> 'a F.t

let view (type a) (type b) (l : (a, b) lens) (x : a) : b =
  let module C = ConstFunctor (struct type t = b end) in
  l {C} id x

let modify (type a) (type b) (l : (a, b) lens) (f  : b -> b) (x : a) : a =
  l {IdFunctor} f x

let set l x = modify l (const x)

let compose (l2 : ('b, 'c) lens) (l1 : ('a, 'b) lens) : ('a, 'c) lens = 
  fun { F : FUNCTOR } f x -> l1 {F} (l2 {F} f) x

let (//) l1 l2 = compose l2 l1

(* Examples *)
type address = { street : string ; number : int}

type person = { name : string; age : int; address : address }

type compnay = { name : string; ceo : person }

(* Lenses *)
let ceo { F : FUNCTOR } f x = F.map (fun ceo -> { x with ceo }) @@ f x.ceo

let address { F : FUNCTOR } f x = 
  F.map (fun address -> { x with address }) @@ f x.address

let street { F : FUNCTOR } f x = 
  F.map (fun street -> { x with street }) @@ f x.street

let company = {
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

let company = set (ceo // address // street) "Wallstreet" company


