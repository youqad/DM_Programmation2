(* expressions régulières sur l'alphabet des valeurs de type 'a *)
type 'a regexp = 
  | Eps                                     (* le mot vide *)
  | Sym of 'a                               (* un mot-lettre *)
  | Alt of 'a regexp * 'a regexp            (* union *)
  | Seq of 'a regexp * 'a regexp            (* concaténation *)
  | Rep of 'a regexp                        (* itération *)

(* application d'un morphisme à une expression régulière *)
let apply_morphism h e = 
  let rec f = function
    | Eps -> Eps
    | Sym c -> h c
    | Alt (e1,e2) -> Alt (f e1 , f e2)
    | Seq (e1,e2) -> Seq (f e1 , f e2)
    | Rep e -> Rep (f e)
  in f e

(* mots sur l'alphabet des valeurs de type 'a *)
type 'a word = 'a list

(* accepteurs *)
type 'a acceptor = 'a regexp -> 'a word -> bool
