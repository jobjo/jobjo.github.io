module I = struct

  module type COMPOSE = sig
    type ('a,'b) t1
    type ('a,'b) t2
    type ('a,'b) t3
    val compose : ('a,'b) t1 -> ('b,'c) t2 -> ('a ,'c) t3
  end

  type ('a , 'b) arrow1 = Arrow1 of ('a -> 'b)

  type ('a , 'b) arrow2 = Arrow2 of ('a -> 'b)

  implicit module Arrow1Arrow1Compose = struct
    type ('a, 'b) t1 = ('a, 'b) arrow1
    type ('a, 'b) t2 = ('a, 'b) arrow1
    type ('a, 'b) t3 = ('a, 'b) arrow1
    let compose (Arrow1 f) (Arrow1 g) = Arrow1 (fun x -> g (f x))
  end

  implicit module Arrow1Arrow2Compose = struct
    type ('a, 'b) t1 = ('a, 'b) arrow1
    type ('a, 'b) t2 = ('a, 'b) arrow2
    type ('a, 'b) t3 = ('a, 'b) arrow2
    let compose (Arrow1 f) (Arrow2 g) = Arrow2 (fun x -> g (f x))
  end

  implicit module Arrow2Arrow2Compose = struct
    type ('a, 'b) t1 = ('a, 'b) arrow2
    type ('a, 'b) t2 = ('a, 'b) arrow2
    type ('a, 'b) t3 = ('a, 'b) arrow2
    let compose (Arrow2 f) (Arrow2 g) = Arrow2 (fun x -> g (f x))
  end

  let (>>) {C : COMPOSE} f g = C.compose f g

  let a1 = Arrow2 List.length
  let a2 = Arrow2 string_of_int
  let a3 = Arrow2 (Printf.printf "%s\n")

  let a4 = a1 >> (a2 >> a3)

end