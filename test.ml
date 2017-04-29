open Regexp

(* TESTS PARTIE 1 *)

let string_of_regexp e =
  let rec f = function
    | Eps -> "."
    | Sym x -> String.make 1 x
    | Alt (e1,e2) -> Format.sprintf "(%s+%s)" (f e1) (f e2)
    | Seq (e1,e2) -> Format.sprintf "%s%s" (f e1) (f e2)
    | Rep e -> Format.sprintf "(%s)*" (f e)
  in f e

let string_of_word w =
  let rec f = function
    | [] -> ""
    | a::w -> (String.make 1 a) ^ (f w)
  in f w

let tests1 = [
  Eps , [] , true;
  Eps , ['a'] , false;
  Sym 'a' , ['a'] , true;
  Sym 'a' , ['b'] , false;
  Sym 'a' , ['a';'b'] , false;
  Sym 'a' , [] , false;
  Alt (Sym 'a',Sym 'b') , ['a'] , true;
  Alt (Sym 'a',Sym 'b') , ['b'] , true;
  Alt (Sym 'a',Sym 'b') , ['c'] , false;
  Seq (Sym 'a',Sym 'b') , ['a';'b'] , true;
  Seq (Sym 'a',Sym 'b') , ['a'] , false;
  Seq (Sym 'a',Sym 'b') , ['b'] , false;
  Seq (Sym 'a',Sym 'b') , [] , false;
  Seq (Sym 'a',Eps) , ['a'] , true;
  Seq (Eps,Sym 'a') , ['a'] , true;
  Seq (Eps,Eps) , [] , true;
  Rep (Sym 'a') , [] , true;
  Rep (Sym 'a') , ['a'] , true;
  Rep (Sym 'a') , ['a';'a'] , true;
  Rep (Sym 'a') , ['a';'a';'a'] , true;
  Rep (Sym 'a') , ['a';'b';'a'] , false

(* A COMPLETER ... *)

]


let treat accept (e,u,b) =
  Format.printf "%s@.%s@."
    (string_of_regexp e)
    (string_of_word u);
  Format.printf "%s@.@." (if accept e u=b then "OK" else "FAILED")

let test1 accept = List.iter (treat accept) tests1



(* TESTS PARTIE 2 *)

(* a completer *)



(* TESTS PARTIE 3 *)

(* a completer *)
