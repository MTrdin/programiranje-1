(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)
 let obrni (a,b) = (b,a)
(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)
 let zamenjaj_drugo (a,b) x = (a,x)

(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)
 let rec vsem_zamenjaj_drugo_z_42 sez_parov = match sez_parov with
   | [] -> []
   | x::xs -> (zamenjaj_drugo x 42 :: vsem_zamenjaj_drugo_z_42 xs)

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)
 let glava = function
   | [] -> None
   |  x::_ -> Some x

(* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)
 let rec uporabi_veckrat f vr n = 
   if n <= 0 then vr
   else uporabi_veckrat f (f vr) (n-1)
(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo =
   | Koren of 'a * 'a drevo list
(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Koren (1, [])
let t' = Koren (2, [t; t])
let t'' = Koren (3, [Koren(-1, []); t'; Koren(0, [])])

(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)
let vrni_gozd (Koren (a, gozd)) = gozd 

(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)
let rec izpisi_vrednosti (Koren (a, gozd)) =
   let rec pomozna f sez = match sez with
      | [] -> ()
      | x::xs -> f x; pomozna f xs
   in
   print_endline (string_of_int a); pomozna print gozd

(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)
let globina Koren (a,gozd) = 
   List.map globina gozd |< List.fold_left max 0 gozd |< (+) 1

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let globoko_drevo globina =
   let rec pomozna acc n =
      if n <= 1 then acc
      else pomozna Koren (n,[acc]) (n-1)
   in
   pomozna Koren(globina,[]) globina
(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let rec zlozi f x0 Koren(a,gozd) = match gozd with
   let rec pomozna acc sez = match sez with
      | [] -> acc
      | drevo::xs -> (
         match drevo with
         | [] -> pomozna acc xs
         | Koren(roza, s)::ts -> pomozna (f x0 roza) s::xs::ts
      )
   in
   pomozna x0 gozd