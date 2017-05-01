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

(* module Weighter : WEIGHTER = functor (W:SEMIRING) -> struct *)

(*     (\* a completer *\) *)
(* end *)


(* TESTS *)

(* a completer *)

module Bool : (SEMIRING with type t = bool) = struct
  type t = bool
  let sum = List.fold_left (||) false
  let prod = List.fold_left (&&) true
end



module WeighS = WeighterSpec(Bool)

let accept = WeighS.eval



(* LANCEMENT DES TESTS -- NE PAS MODIFIER SOUS CETTE LIGNE *)

let () = test3 accept
