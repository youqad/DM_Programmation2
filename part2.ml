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

  (* pour des raisons d'efficacité (je ne voulais pas récurremment faire appel à des
     fonctions récursives linéaires "eps_in" ("contient epsilon") et "rightmost_value"
     (pondération du noeud Sym le plus à droite dans l'arbre syntaxique)) j'ai
     quelque peu changé la sémantique de "'a state" :
     - dans bool * ('a * bool) regexp :
         - le premier booléen indique si une marque peut se propager du noeud
           Sym marqué le plus à droite dans l'arbre syntaxique vers la FIN de l'expression
           régulière (ce qui implique qu'on peut rejoindre un état final par des epsilon-transitions)
         - pour chaque couple (a, bool) : bool indique s'il y a une marque juste AVANT la lettre "a"
           (ce qui implique qu'il est loisible de lire la lettre "a")
     Par ailleurs, la fonction "eps_closure" pousse le plus à droite possible les marques
     existantes (elle effectue une epsilon-clôture)
  *)

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

  let next (st : 'a state) (a :'a) : 'a state = next_reg (snd st) a

  let final (st : 'a state) : bool = fst st

end


(* UTILISATION DU MODULE ET DES FONCTEURS *)

module Acc = Acceptor.Make(Part2)

let accept (e : 'a regexp) (u: 'a word) : bool = Acc.accept e u


(* LANCEMENT DES TESTS *)


(* Part2 seulement :

let () = test1 accept "part2"

*)

(* COMPARE PART1 & PART2 *)

let () = testCompare1and2 (Part1.accept) accept


(* Cette méthode ne devient significativement plus efficace que sur les plus gros exemples :

   faire "./part2.byte > comparison_part1-part2.txt"
   et regarder les derniers exemples.

*)
