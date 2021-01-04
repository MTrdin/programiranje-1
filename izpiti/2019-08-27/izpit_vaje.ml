let odstej_trojici tr1 tr2 =
    let (a,b,c) = tr1 in
    let (x,y,z) = tr2 in
    (a-x, b-y, c-z)


(*List.init*)
let max_rezultat_do_n f n =
    let rec aux trenutni_max trenutni_i =
        if trenutni_i < 0 then trenutni_max
        else (
            let v = trenutni_i in
            aux (max v trenutni_max) (trenutni_i -1)
        )
    in
    aux (f n) (n - 1)

let rec pocisti_seznam list = match list with
    | [] -> []
    | (Some x)::ostalo -> x::pocisti_seznam ostalo
    | _::ostalo -> pocisti_seznam ostalo
(*to ni repno rekurzivna *)

let pocisti2 l =
    letz rec aux acc = function
    | [] -> []
    | (Some x):: xs -> aux (x::acc) xs
    | _ :: xs -> aux acc xs
    in aux [] l

let rec je_urejen = function
    | [] | [_] -> true


let preveri_urejenost l =
    let aux min_sodo max_liho = function
    | [] -> true
    | x::xs -> if x mod 2 = 0 then x > min_sodo && aux x max_liho xs else x < max_liho && aux min_sodo x xs
    in
    aux (-9999) 9999 l


(*----------------------------------*)

type 'a gnezdenje =
    | Element of 'a
    | Podseznam of 'a gnezdenje list

let gnezdenje_primer =
    Podseznam(Element(1), Element(2),Podseznam(Element(3), Podseznam(4), Podseznam(None)), Podseznam(Element(5)))

Podseznam[
    Element 1;
    Element 2;
    Podseznam (
        [Element 3; Podseznam [Element 4]; Podseznam [] ]
    );
    Podseznam [Element 5]
]


let rec najvecja_globina_gnezdenja g =
    match g with
    | Element _ -> 0
    | Podseznam xs -> 1 + (List.fold_left max 1 (List.map najvecja_globina_gnezdenja xs))

let najvecja_globina g_list =
    1 + (List.fold_left max 1 (list.map najvecja_globina_gnezdenja g_list)) 

(*pomembna fuja*)
let rec preslikaj f g = match g with
    | Element x -> Element (f x)
    | Podseznam xs -> Podseznam((list.map (preslikaj f)) xs)

let rec splosci = function
    | Element x -> [x]
    | Podseznam xs -> 
        let splosceni = List.map splosci xs in
        List.fold_left (@) [] splosceni

let rec alternirajoci_konstruktorji = function
    | [] -> true
    | [x] -> true
    | Element _:: Podseznam p::xs -> alternirajoci_konstruktorji ((Podseznam p):: xs)
    | Podseznam _:: Element p::xs -> alternirajoci_konstruktorji ((Podseznam p):: xs)
    | _ -> false

let rec zlozi_gnezdenje f acc g = match g with
    | Element x -> f acc x
    | Podseznam l -> 
        List.fold_left (zlozi_gnezdenje f) acc l

let zlozi_preko_gnezdenja f acc g_list =
    zlozi_gnezdenje f acc (Podseznam g_list)