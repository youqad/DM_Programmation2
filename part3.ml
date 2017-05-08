open Regexp
open Wregexp
open Test

(* PONDERATEUR CONSTRUIT EN SE BASANT SUR LA DEFINITION INDUCTIVE *)

module WeighterSpec : WEIGHTER = functor (W:SEMIRING) -> struct
  let zero = W.sum []
  let one = W.prod []
  let rec splits2 acc = function
    | [] -> [acc, []]
    | h::t as l -> (acc, l) :: (splits2 (acc @ [h]) t)

  let rec splits n acc l = let len = List.length l in
    match l with
    | _ when n=1 -> [[acc@l]]
    | _ when len <= n-1 ->
      if len = n-1
      then [acc :: (List.map (fun x -> [x]) l)]
      else []
    | h::t ->
      (List.map
         (fun l' -> acc::l')
         (splits (n-1) [h] t)) @ (splits n (acc @ [h]) t)
    | _ -> failwith "Error splits"

  let rec eval wreg u =
     match wreg, u with
    | Eps, [] -> one
    | Eps, _ -> zero
    | Sym f, [a] -> f(a)
    | Sym f, _ -> zero
    | Alt (e1, e2), _ -> W.sum [eval e1 u; eval e2 u]
    | Seq (e1, e2), _ ->
      W.sum (List.map
               (fun (u1, u2) -> W.prod [eval e1 u1; eval e2 u2])
               (splits2 [] u))
    | Rep _, [] -> one
    | Rep e, h::t ->
      let rec partitions = function
        | 1 -> [[u]]
        | n -> (splits n [h] t) @ partitions (n-1) in
      W.sum (List.map
               (fun l' -> W.prod (List.map (eval e) l'))
               (partitions (List.length u)))
end


(* PONDERATEUR CONSTRUIT EN GENERALISANT LA PARTIE 2 *)


module WeighterSpecFast : WEIGHTER = functor (W:SEMIRING) -> struct
  let zero = W.sum []
  let one = W.prod []

  (* on procède de manière analogue à la partie 2 :
         - false devient maintenant zero
         - true correspond à un élément du semi-anneau non nul
     La epsilon-clôture ne se contente plus de "pousser les marques"
     le plus à droite possible : le paramètre b est un poids (du semi-anneau)
     qui essaye de se propager le plus à droite possible dans l'expression régulière
  *)

  let rec eval wreg u =
    let rec semiringize = function
      | Eps -> Eps
      | Sym a -> Sym (a, zero)
      | Alt (reg1, reg2) -> Alt (semiringize reg1, semiringize reg2)
      | Seq (reg1, reg2) -> Seq (semiringize reg1, semiringize reg2)
      | Rep reg -> Rep (semiringize reg) in
    let rec next_reg reg a =
      match reg with
      | Eps -> zero, Eps
      | Sym (f, t) -> W.prod [t; f a], Sym (f, zero)
      | Alt (reg1, reg2) -> plus (next_reg reg1 a) (next_reg reg2 a)
      | Seq (reg1, reg2) -> concat (next_reg reg1 a) (next_reg reg2 a)
      | Rep reg1 -> star (next_reg reg1 a)
    and plus (b1, r1) (b2, r2) = W.sum [b1; b2], Alt (r1, r2)
    and concat (b1, r1) (b2, r2) = let b2', r2' = eps_closure r2 b1 in
      W.sum [b2; b2'], Seq (r1, r2')
    and star (b, r) = let b', r' = eps_closure r b in
      W.sum [b'; b], Rep r'
    and eps_closure reg b = match reg with
      | Eps -> b, Eps
      | Sym (f, b') -> zero, Sym (f, W.sum [b'; b])
      | Alt (reg1, reg2) -> plus (eps_closure reg1 b) (eps_closure reg2 b)
      | Seq (reg1, reg2) -> concat (eps_closure reg1 b) (zero, reg2)
      | Rep r -> let b', r' = eps_closure r b in W.sum [b'; b], Rep r' in
    let init reg = eps_closure (semiringize reg) one in
    let next st a = next_reg (snd st) a in
    let final st = fst st in
    let accept reg word =
      let ending =
        List.fold_left (fun st a -> next st a) (init reg) word in
      final ending in
    accept wreg u
end


(* TESTS *)


module Bool : (SEMIRING with type t = bool) = struct
  type t = bool
  let sum = List.fold_left (||) false
  let prod = List.fold_left (&&) true
end

module Nat : (SEMIRING with type t = int) = struct
  type t = int
  let sum = List.fold_left (+) 0
  let prod = List.fold_left ( * ) 1
end

module WeighterBool = WeighterSpec(Bool)

module WeighterNat = WeighterSpec(Nat)

module WeighterBoolFast = WeighterSpecFast(Bool)

module WeighterNatFast = WeighterSpecFast(Nat)


(* LANCEMENT DES TESTS *)

let () = testBool (WeighterBool.eval) (WeighterBoolFast.eval)

let () = testNat (WeighterNat.eval) (WeighterNatFast.eval)
