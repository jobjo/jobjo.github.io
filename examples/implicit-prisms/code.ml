module I = struct

  let id x = x
  let const x _ = x

  module Type = struct
    module type S = sig type t end
  end

  module Functor = struct
    module type S = sig
      type 'a t
      val map : ('a -> 'b) -> 'a t -> 'b t
    end
  end

  module Applicative = struct
    module type S = sig
      type 'a t
      val map : ('a -> 'b) -> 'a t -> 'b t
      val pure : 'a -> 'a t
      val apply : ('a -> 'b) t -> 'a t -> 'b t
    end
  end

  module IdFunctor = struct
    type 'a t = 'a
    let map f x = f x
  end

  module ConstFunctor (T : Type.S) = struct
    type 'a t = T.t
    let map _ x = x
  end

  module OptionFunctor = struct
    type 'a t = 'a option
    let map f = function
      | Some x  -> Some (f x)
      | None    -> None
  end

  module OptionApplicative = struct
    type 'a t = 'a option
    let map f = function
      | Some x -> Some (f x)
      | None -> None

    let pure x = Some x
    let apply f x =
      match f, x with
      | Some f, Some x  -> Some (f x)
      | _               -> None
  end

  module ConstApplicative (T : Type.S) = struct
    type 'a t = T.t option
    let pure _    = None
    let map _ x   = x
    let apply _ x = x
  end

  (* module ApplicativeFunctor { A : Applicative.S } = struct
    type 'a t = 'a A.t
    let map f x = A.apply (A.pure f) x
  end *)


  module Lens = struct

    type ('a, 'b) t = { F : Functor.S } -> ('b -> 'b F.t) -> 'a -> 'a F.t

    let view (type a) (type b) (l : (a, b) t) (x : a) : b =
      let module C = ConstFunctor (struct type t = b end) in
      l {C} id x

    let modify (type a) (type b) (l : (a, b) t) (f  : b -> b) (x : a) : a =
      l {IdFunctor} f x

    let set l x = modify l (const x)

    let compose (l2 : ('b, 'c) t) (l1 : ('a, 'b) t) : ('a, 'c) t =
      fun { F : Functor.S} f x -> l1 {F} (l2 {F} f) x

    let (//) l1 l2 = compose l2 l1

  end

  (* Prism *)
  module Prism = struct

    type ('a, 'b) t = { A : Applicative.S } -> ('b -> 'b A.t) -> 'a -> 'a A.t

    let view (type a) (type b) (p : (a, b) t) (x : a) : b option =
      let module C = ConstApplicative (struct type t = b end) in
      p {C} (fun y -> Some y) x

    let modify (type a) (type b) (p : (a, b) t) (f  : b -> b) (x : a) =
      let module OA = OptionApplicative in
      match p {OA} (fun x -> OA.pure (f x)) x with
      | Some y  -> y
      | None    -> x

    let set l x = modify l (const x)

    let compose (l2 : ('b, 'c) t) (l1 : ('a, 'b) t) : ('a, 'c) t =
      fun { A : Applicative.S} f x -> l1 {A} (l2 {A} f) x

    let (/?) l1 l2 = compose l2 l1
  end

let prism (type a) (type b) (l : (a, b) Lens.t) : (a, b) Prism.t =
  fun {A : Applicative.S} f x -> l {A} f x

  let (/?) p1 p2 = Prism.compose p2 p1

  (**************************************************************
   * Examples
   **************************************************************)
  type physical_address = { street : string ; number : int}

  type address =
    | Physical of physical_address
    | Email of string

  type person = { name : string; age : int; address : address option}

  type company = { name : string; ceo : person }

  (**************************************************************)
  let ceo : (company, person) Lens.t = fun { F : Functor.S } f x ->
    F.map (fun ceo -> { x with ceo }) @@ f x.ceo

  let address { F : Functor.S} f x =
    F.map (fun address -> { x with address }) @@ f x.address

  let some : ('a option, 'a) Prism.t = fun {A : Applicative.S} f x ->
    match x with
    | Some y -> A.apply (A.pure (fun x -> Some x)) (f y)
    | None   -> A.pure None

  let street { F : Functor.S} f x =
    F.map (fun street -> { x with street }) @@ f x.street

  let physical_address {A : Applicative.S} f x =
    match x with
    | Physical p -> A.apply (A.pure (fun pa -> Physical pa)) (f p)
    | Email _    -> A.pure x

  let ceo_street =
    prism ceo /? prism address /? some /? physical_address /? prism street

  let c =
    {
      name = "Lens Inc";
      ceo =
        {
          name = "Mary";
          age = 54;
          address = Some (Physical { street = "Highstreet"; number = 13; })
       }
    }

  let view p = Prism.view p
  let modify p = Prism.modify p
  let set p = Prism.set p

  (*
  module Compose  = struct
    module type S = sig
      type ('a,'b) t1
      type ('a,'b) t2
      type ('a,'b) t3
      val compose : ('a,'b) t1 -> ('b,'c) t2 -> ('a ,'c) t3
    end
    let (//) {C : S} f g = C.compose f g
  end



  implicit module LensCompose : Compose.S with
                                      type ('a,'b) t1 = ('a,'b) Lens.t
                                  and type ('a,'b) t2 = ('a,'b) Lens.t
                                  and type ('a,'b) t3 = ('a,'b) Lens.t = struct
    type ('a,'b) t1 = ('a,'b) Lens.t
    type ('a,'b) t2 = ('a,'b) Lens.t
    type ('a,'b) t3 = ('a,'b) Lens.t
    let compose (l1 : ('a, 'b) t1)
                (l2 : ('b, 'c) t2) :
                ('a, 'c) t3 =
      fun { F : Functor.S } -> l2 {F} >> l1 {F}
  end

  implicit module PrismCompose : Compose.S with
                                  type ('a,'b) t1 = ('a,'b) Prism.t and
                                  type ('a,'b) t2 = ('a,'b) Prism.t and
                                  type ('a,'b) t3 = ('a,'b) Prism.t = struct
    type ('a,'b) t1 = ('a,'b) Prism.t
    type ('a,'b) t2 = ('a,'b) Prism.t
    type ('a,'b) t3 = ('a,'b) Prism.t
    let compose (l1 : ('a, 'b) t1)
                (l2 : ('b, 'c) t2) :
                ('a, 'c) t3 =
      fun { A : Applicative.S} -> l2 {A} >> l1 {A}
  end

  implicit module LensPrismCompose = struct
    type ('a,'b) t1 = ('a,'b) Lens.t
    type ('a,'b) t2 = ('a,'b) Prism.t
    type ('a,'b) t3 = ('a,'b) Prism.t
    let compose (l1 : ('a, 'b) Lens.t)
                (l2 : ('b, 'c) Prism.t) :
                ('a, 'c) Prism.t =
      fun { A : Applicative.S} ->
        let module F = FunctorApplicative {A} in
        l2 {A} >> l1 {F}
  end

  implicit module PrismLensCompose = struct
    type ('a,'b) t1 = ('a,'b) Prism.t
    type ('a,'b) t2 = ('a,'b) Lens.t
    type ('a,'b) t3 = ('a,'b) Prism.t
    let compose (l1 : ('a, 'b) Prism.t) (l2 : ('b, 'c) Lens.t) : ('a, 'c) Prism.t =
      fun {A : Applicative.S} ->
        let module F = FunctorApplicative {A} in
        l2 {F} >> l1 {A}
  end
  *)
end
