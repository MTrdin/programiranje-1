
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

let square x = x*x

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

let middle_of_triple (_, druga, _) = druga

let middle_of_triple_let triple =
  let (prva, druga, tretja) = triple in 
  druga

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let starting_element sez = match sez with
  | [] -> failwith "podal si prazen seznam"
  | prvi::_ -> prvi
(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply sez = match sez with
  | e::rest -> e * (multiply rest)
  | [] -> 1

let rec multiply_func = function
  | e::rest -> (multiply_func rest)
  | [] -> 1
(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs = function
  | [] -> []
  |  (prvi, drugi)::rest -> (prvi + drugi) :: (sum_int_pairs rest)
  

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k list = match list with
  | [] -> failwith "prekratek seznam"
  | a :: rest -> (if k <= 0 then a else get (k-1) rest)

(*se druga moznost*)

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)
let rec double_moja sez = match sez with
  | []-> []
  | x::ostalo -> [x;x] @ doubl ostalo

let rec double = function
  | [] -> []
  | x::xs -> x :: (x:: (double xs))

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k = function
  | [] -> [x] (* k je nepomemben*)
  | e::rest -> (
    if k <= 0 then x::e::rest
    else e:: (insert x (k-1) rest)
  )

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)


(* tule ne deluje za negativne k-je *)
let rec divide k list = match (k, list) with
  | (0, d) -> ([], d)
  | (k', x::xs) -> (
    let (prvi, drugi) = divide (k' - 1) xs in
    (x::prvi,drugi)
  )
  |(_, []) -> ([], [])

let rec divide k list = 
  match (k, list) with
  | (_, []) -> ([], [])
  | (k, list) when k <= 0 -> ([], list)
  | (k, x :: xs) ->
      let (list1, list2) = divide (k - 1) xs in
	    (x :: list1, list2)


(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list = match (n, list) with
  | (0, a) -> a
  | (n', a::rest) -> (rotate (n-1) (rest @ [a]))
  | (_, []) -> failwith "tole je pa napaka"

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list = match list with
  | [] -> []
  | a::rest -> if (a = x) then (remove x rest) else a::(remove x rest)

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome list = 
  let rec obrni l = match l with
    | [] -> []
    | a::xs -> (obrni xs) @ [a]
  in 
  list = obrni list

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components l1 l2 = match (l1, l2) with
  | ([], _) -> []
  | (_, []) -> []
  | (x::xs, y::ys) -> (if x > y then x else y) :: max_on_components xs ys

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest = function
  | [] -> 0
  | x::y::xs -> x + y
  | x::xs -> x

let second_largest list =
  let rec largest = function
    | [] -> failwith "List is too short."
	  | x :: [] -> x
	  | x :: xs -> max x (largest xs)
  in
  largest (remove (largest list) list)