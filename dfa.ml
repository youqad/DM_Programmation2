open Regexp

(* définition abstraite d'une construction d'automate déterministe
   à partir d'une expression régulière *)

module type DFA = sig
  type 'a state
  val init : 'a regexp -> 'a state
  val next : 'a state -> 'a -> 'a state
  val final : 'a state -> bool
end


(* accepteur associé à une construction d'automate *)
module type ACCEPTOR = sig
  val accept : 'a acceptor
end

module Acceptor = struct

  module Make (DFA:DFA) : ACCEPTOR = struct
    let accept reg word =
      let ending_state =
        List.fold_left (fun st a -> next st a) (init reg) word in
      final ending_state
  end

end
