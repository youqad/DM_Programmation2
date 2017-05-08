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
  Rep (Sym 'a') , ['a';'b';'a'] , false;

  (* A COMPLETER ... *)

  Seq(Seq(Rep (Alt(Seq (Sym 'a',Sym 'a'), Sym 'b')), Alt (Sym 'a', Eps)), Sym 'b') , ['a';'a';'a';'b'] , true;
  Seq(Seq(Rep (Alt(Seq (Sym 'a',Sym 'a'), Sym 'b')), Alt (Sym 'a', Eps)), Sym 'b'), ['a';'b';'a'] , false;
  Alt((Sym 'a') , (Rep (Sym 'a'))), ['a'], true;
  Seq(Alt((Sym 'a') , (Rep (Sym 'a'))), Alt((Sym 'b') , (Rep (Sym 'b')))), ['a';'b'], true;
  Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), ['a'; 'b'; 'b'], true;
  Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), ['b'; 'a'], true;
  Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), ['a'; 'b'; 'a'], false;
  Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')), ['a'; 'b'; 'a'], false;
  Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), ['b'; 'a'; 'b'; 'a'], true;
  Rep (Seq(Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), Sym 'c')), ['b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'c'; 'a'; 'a'; 'c'], true;
  Rep (Alt (Seq(Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), Sym 'c'), Seq (Sym 'c', Sym 'd'))), ['b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'c'; 'a'; 'a'; 'c'; 'c'; 'd'], true;
  Rep (Seq (Rep (Seq(Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), Sym 'c')), Sym 'b')), ['b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'c'; 'a'; 'a'; 'b'; 'a'; 'b'; 'a'; 'c'; 'b'; 'a'; 'a'; 'c'; 'b'], true;
  Seq( Seq(Seq(Rep (Alt(Seq (Sym 'a',Sym 'a'), Sym 'b')), Alt (Sym 'a', Eps)), Sym 'b'), Rep (Seq (Rep (Seq(Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), Sym 'c')), Sym 'b'))), ['a';'a';'a';'b';'b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'c'; 'a'; 'a'; 'b'; 'a'; 'b'; 'a'; 'c'; 'b'; 'a'; 'a'; 'c'; 'b'], true;
   Seq(Rep (Seq(Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), Sym 'c')), Rep (Seq (Rep (Seq(Alt (Seq (Sym 'a', Rep (Sym 'b')), Rep (Seq (Alt (Sym 'a', Sym 'b'), Sym 'a')) ), Sym 'c')), Sym 'b'))), ['b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'c'; 'a'; 'a'; 'c'; 'b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'c'; 'a'; 'a'; 'b'; 'a'; 'b'; 'a'; 'c'; 'b'; 'a'; 'a'; 'c'; 'b'], true;
]


let hr = "________________________________________" (* horizontal rule *)
let time label f x y =
    let t = Sys.time() in
    let fxy = f x y in
    Printf.printf "Temps d'exécution %s : %fs\n" label (Sys.time() -. t);
    fxy

let treat accept label (e,u,b) =
  Format.printf "%s@.%s@.%s@." hr
    (string_of_regexp e)
    (string_of_word u);
  Format.printf "%s@.@." (if time label accept e u = b then "OK" else "FAILED")

let test1 accept label = List.iter (treat accept label) tests1



(* TESTS PARTIE 2 *)

let treatCompare accept1 accept2 (e,u,b) =
  Format.printf "%s@.%s@.%s@." hr
    (string_of_regexp e)
    (string_of_word u);
  Format.printf "%s@.@." (if time "part1" accept1 e u = b then "OK" else "FAILED");
  Format.printf "%s@.@." (if time "part2" accept2 e u = b then "OK" else "FAILED")


let test2 accept label = List.iter (treat accept label) tests1

let testCompare1and2 accept1 accept2 = List.iter (treatCompare accept1 accept2) tests1

(* TESTS PARTIE 3 *)


let wregexp_of_regexpNat = apply_morphism (fun a x -> if a=x then 1 else 0)


(* Semi-anneau : Booléens *)

let treatBool accept acceptFast (e,u,b) =
  Format.printf "%s@.%s@.%s@." hr
    (string_of_regexp e)
    (string_of_word u);
  Format.printf "%s@.@." (if time "pondérateur booléen efficace" acceptFast (apply_morphism (=) e) u=b then "OK" else "FAILED");
  try Format.printf "%s@.@." (if time "pondérateur booléen lent" accept (apply_morphism (=) e) u=b then "OK" else "FAILED")
  with Stack_overflow -> Format.printf "%s@.@." "pondérateur booléen lent : Stack overflow !"

let testBool accept acceptFast = List.iter (treatBool accept acceptFast) tests1

(* ---------------------------------------------------------------------------- *)

(* Semi-anneau : Entiers naturels *)

let treatNat accept acceptFast (e,u,b) =
  Format.printf "%s@.%s@.%s@." hr
    (string_of_regexp e)
    (string_of_word u);
  Format.printf "%d@.@." (time "pondérateur naturel efficace" acceptFast (wregexp_of_regexpNat e) u);
  try Format.printf "%d@.@." (time "pondérateur naturel lent" accept (wregexp_of_regexpNat e) u)
  with Stack_overflow -> Format.printf "%s@.@." "pondérateur booléen lent : Stack overflow !"

let testNat accept acceptFast = List.iter (treatNat accept acceptFast) tests1
