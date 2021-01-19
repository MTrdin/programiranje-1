let razlika_kvadratov a b = 
    let kvadrat_vsote = (a+b)*(a+b) in
    let vsota_kvadratov = (a*a) + (b*b)in
    kvadrat_vsote - vsota_kvadratov

let uporabi_na_paru f (a,b) = (f a, f b)

let rec ponovi_seznam n sez =
    if n <= 0 then []
    else
        sez @ ponovi_seznam (n-1) sez 

let rec razdeli sez =
    let rec pomozna acc_l acc_d s = match s with
        | [] -> (List.rev acc_l,List.rev acc_d)
        | x::xs -> if x < 0 then pomozna (x::acc_l) acc_d xs
                else pomozna acc_l (x::acc_d) xs
    in
    pomozna [] [] sez

(*--------------------------------*)

type drevo =
    | Prazno
    | Vozlisce of drevo * int * drevo

let rec monotona_pot drevo = match drevo with
    | Prazno -> []
    | Vozlisce (l, st, d) -> 
        let prvi = List.rev (padajoce_z st l) @ [st] @ narascajoce_zap st d in
        let drugi = List.rev (narascajoce_zap st l) @ [st] @ padajoce_z st d in
        let long_skupaj = dalsi_od prvi drugi in
        let long_poddrevesa = dalsi_od (monotona_pot l) (monotona_pot d) in
        dalsi_od long_skupaj long_poddrevesa

let rec padajoce_z sredina drevo = match drevo with
    | Prazno -> []
    | Vozlisce (l, st, d) when st > sredina -> []
    | Vozlisce (l, st, d) -> x :: dalsi_od (padajoce_z x l) (padajoce_z x d)

let rec narascajoce_zap sredina = function
  | Prazno -> []
  | Vozlisce (l, x, r) when x < sredina -> []
  | Vozlisce (l, x, r) -> x :: dalsi_od (narascajoce_zap x l) (narascajoce_zap x r)

let dalsi_od l1 l2 = 
    if List.length l1 < List.length l2 then l2
    else l1

(*------------------------------------------------------*)

type 'a veriga = 
    | Filter of ('a -> bool) * 'a list * 'a veriga
    | Ostalo of 'a list

let test = 
    Filter ((fun x -> x < 0), [],
    Filter((fun x -> x < 10), [],
    Ostalo []))

let rec vstavi a veriga = match veriga with
    | Ostalo sez -> Ostalo a :: sez
    | Filter (f, sez, v) -> 
        if f a then Filter (f, a::sez, v)
        else Filter (f, sez, vstavi a v)

let rec poisci a veriga = match veriga with
    | Ostalo sez -> List.mem a sez
    | Filter (f, sez, v) -> if not f a then poisci a v else List.mem a sez
(*| Filter (f, sez, veriga) when f x -> List.mem x sez
  | Filter (_, _, veriga) -> poisci x veriga*)

let rec izprazni_filtre veriga = match veriga with
    | Ostalo sez -> (Ostalo [], sez)
    | Filter (f, sez, v) -> 
        let prazna_veriga, ostanek = izprazni_filtre v in
        (Filter (f, [], prazna_veriga), sez @ ostanek)

let rec f_na_seznamu f sez = match sez with
    | [] -> []
    | x::xs -> if f x then x::f_na_seznamu f xs else f_na_seznamu f xs

let rec f_na_seznamu_ost f sez = match sez with
    | [] -> []
    | x::xs when f x -> f_na_seznamu_ost f xs
    | x::xs -> x::f_na_seznamu_ost f xs

(*ni cis prov*)
let rec dodaj_filter f veriga = match veriga with
    | Ostalo sez -> 
        let sez_ki_zadosca_f = f_na_seznamu f sez in
        let sez_ostanka = f_na_seznamu_ost f sez in
        Filter (f, sez_ki_zadosca_f, sez_ostanka)
    | Filter (g, sez, v) -> 
        let sez_ki_zadosca_f = f_na_seznamu f sez in
        let sez_ostanka = f_na_seznamu_ost f sez in
        let nova_veriga = Filter (g, sez_ostanka, v) in
        Filter (f, sez_ki_zadosca_f, nova_veriga )