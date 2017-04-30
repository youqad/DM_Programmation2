open Regexp
open Dfa
open Test

module Part1 : DFA = struct

  type 'a state = 'a regexp option
  let init (reg : 'a regexp) : 'a state = Some reg
  let rec eps_in reg = match reg with
    | Eps | Rep _ -> true
    | Sym _ -> false
    | Alt (reg1, reg2) -> eps_in reg1 || eps_in reg2
    | Seq (reg1, reg2) -> eps_in reg1 && eps_in reg2
  let rec next (st : 'a state) (a :'a) : 'a state = match st with
    | None | Some Eps -> None
    | Some (Sym b) -> if b = a then Some Eps else None
    | Some Alt (reg1, reg2) -> let st1 = next (Some reg1) a
      and st2 = next (Some reg2) a in (
        match st1, st2 with
        | None, None -> None
        | None, st2 -> st2
        | st1, None -> st1
        | Some r1, Some r2 -> Some (Alt (r1, r2))
      )
    | Some (Seq (reg1, reg2)) -> let st1 = next (Some reg1) a in
      if eps_in reg1 then
        (
          let st2 = next (Some reg2) a in
          match st1, st2 with
          | None, None -> None
          | None, st2 -> st2
          | Some r1, None -> Some (Seq (r1, reg2))
          | Some r1, Some r2 -> Some (Alt (Seq (r1, reg2), r2))
        )
      else
        (
          match st1 with
          | None -> None
          | Some r1 -> Some (Seq (r1, reg2))
        )
    | Some (Rep reg) -> let st = next (Some reg) a in (
      match st with
          | None -> None
          | Some r -> Some (Seq (r, Rep reg))
      )

  let final (st : 'a state) : bool = match st with
    | None -> false
    | Some reg -> eps_in reg
end


(* UTILISATION DU MODULE ET DES FONCTEURS *)

module Acc = Acceptor.Make(Part1)

let accept (e : 'a regexp) (u: 'a word) : bool = Acc.accept e u



(* LANCEMENT DES TESTS -- NE PAS MODIFIER SOUS CETTE LIGNE *)

let () = test1 accept
