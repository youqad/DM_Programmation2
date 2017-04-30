open Regexp
open Dfa
open Test

module Part2 : DFA = struct

  type 'a state =  bool * ('a * bool) regexp

  let rec booleanize = function
    | Eps -> Eps
    | Sym a -> Sym (a, false)
    | Alt (reg1, reg2) -> Alt (booleanize reg1, booleanize reg2)
    | Seq (reg1, reg2) -> Seq (booleanize reg1, booleanize reg2)
    | Rep reg -> Rep (booleanize reg)

  let rec next_reg (reg : ('a * bool) regexp) (a :'a) : 'a state =
    match reg with
    | (Eps as r) | (Sym (_, false) as r) -> false, r
    | Sym (b, true) -> if b = a
      then true, Sym (b, false)
      else false, Sym (b, false)
    | Alt (reg1, reg2) -> plus (next_reg reg1 a) (next_reg reg2 a)
    | Seq (reg1, reg2) -> concat (next_reg reg1 a) (next_reg reg2 a)
    | Rep reg -> star (next_reg reg a)
  and plus (b1, r1) (b2, r2) = (b1 || b2), Alt (r1, r2)
  and concat (b1, r1) (b2, r2) = match b1 with
    | false -> b2, Seq (r1, r2)
    | true -> let b2', r2' = eps_closure r2 in
      (b2 || b2'), Seq (r1, r2')
  and star (b, r) = match b with
    | false -> false, Rep r
    | true -> let b', r' = eps_closure r in
      true, Rep r'
  and eps_closure = function
    | Eps -> true, Eps
    | Sym (b, _) -> false, Sym (b, true)
    | Alt (reg1, reg2) -> plus (eps_closure reg1) (eps_closure reg2)
    | Seq (reg1, reg2) -> concat (eps_closure reg1) (false, reg2)
    | Rep reg -> let _, r = eps_closure reg in true, Rep r

  let init (reg : 'a regexp) : 'a state = eps_closure (booleanize reg)

  (* let rec rightmost (reg : ('a * bool) regexp) : bool = match reg with *)
  (*   | Eps -> false *)
  (*   | Sym (_, bool) -> bool *)
  (*   | Alt (reg1, reg2) -> rightmost reg1 || rightmost reg2 *)
  (*   | Seq (reg1, reg2) -> if eps_in reg2 *)
  (*     then rightmost reg1 || rightmost reg2 *)
  (*     else rightmost reg2 *)
  (*   | Rep reg -> rightmost reg *)

  (* let rec no_pointer (reg : ('a * bool) regexp) : bool = match reg with *)
  (*   | Eps -> true *)
  (*   | Sym (_, bool) -> not bool *)
  (*   | Alt (reg1, reg2) | Seq (reg1, reg2) -> no_pointer reg1 && no_pointer reg2 *)
  (*   | Rep reg -> no_pointer reg *)

  let next (st : 'a state) (a :'a) : 'a state = next_reg (snd st) a

  let final (st : 'a state) : bool = fst st

end


(* UTILISATION DU MODULE ET DES FONCTEURS *)

module Acc = Acceptor.Make(Part2)

let accept (e : 'a regexp) (u: 'a word) : bool = Acc.accept e u



(* LANCEMENT DES TESTS -- NE PAS MODIFIER SOUS CETTE LIGNE *)

let () = test2 accept
