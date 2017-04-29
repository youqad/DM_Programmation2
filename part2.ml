open Regexp
open Dfa
open Test

module Part2 : DFA = struct

  type 'a state =  bool * ('a * bool) regexp


(* A COMPLETER ...*)

end


(* UTILISATION DU MODULE ET DES FONCTEURS *)

let accept e u = 

  (* A COMPLETER *)

  true




(* LANCEMENT DES TESTS -- NE PAS MODIFIER SOUS CETTE LIGNE *)

let () = test2 accept
