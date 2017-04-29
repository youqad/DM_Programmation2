open Regexp

(* expression régulières pondérées *)

type ('sym,'weight) wregexp = ('sym -> 'weight) regexp


(* pondérateur *)

type ('sym,'weight) weighter = ('sym,'weight) wregexp -> 'sym word -> 'weight


(* anneau de poids *)

module type SEMIRING = sig
  type t
  val sum : t list -> t
  val prod : t list -> t
end


(* ponderateur associe à un anneau de poids *)

module type WEIGHTER = functor (W:SEMIRING) -> sig
  val eval : ('a,W.t) weighter
end




