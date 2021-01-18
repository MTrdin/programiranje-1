let podvoji_vsoto a b = 2 * (a + b)

let povsod_vecji (a0,a1,a2) (b0,b1,b2) =
    if a0 > b0 && a1 > b1 && a2 > b2 then true
    else false

let uporabu_ce_lahko f a = match a with
    | None -> None
    | Some a -> Some (f a)

let pojavi_dvakrat_ne_deluje a sez =
    let rec naredi_pare e s = match s with
        | [] -> []
        | x::xs -> (e,x):: naredi_pare e xs
    in
    let pari = naredi_pare a sez in
    sez_enakh = List.filter (fun (b, y) -> b = y) pari
    d = List.length sez_enakh
    if d = 2 then true else false

let rec pojavi_dvakrat a sez =
    let rec pojavi_kolikokrat = function
        | [] -> 0
        | x::xs -> if a = x then 1 + pojavi_kolikokrat xs else pojavi_kolikokrat xs
    in
    pojavi_kolikokrat sez = 2

let izracunaj_v_tocki a sez_f =
    let rec pomozna acc sez = match sez with
        | [] -> List.rev acc
        | f::ostalo -> pomozna ((f a)::acc) ostalo
    in
    pomozna [] sez_f

let exponent x potenca =
    let rec pomozna acc p =
        if p <= 0 then acc else pomozna (x*acc) (p-1)
    in
    pomozna 1 potenca


(*----------------------------*)

type 'a mm_drevo = 
    | Prazno
    | Vozlisce of 'a mm_drevo * 'a * int * 'a mm_drevo

let rec vstavi element drevo = match drevo with
    | Prano -> Vozlisce (Prazno, element, 1, Prazno)
    | Vozlisce (levo, a, st, desno) -> 
        if element < a then vstavi element levo
        else if element > a then vstavi element desno
        else Vozlisce (levo, a, st + 1, desno)

(*iz resitev*)
let rec vstavi x = function
  | Empty -> Node (Empty, x, 1, Empty)
  | Node (lt, y, count, rt) when y = x -> Node(lt, y, count + 1, rt)
  | Node (lt, y, count, rt) when y > x -> Node(vstavi x lt, y, count, rt)
  | Node (lt, y, count, rt) -> Node(lt, y, count, vstavi x rt)

let rec multimnozica_iz_seznama sez = 
    let rec pomozna drevo_acc s = match s with
        | [] -> drevo_acc
        | a::ostalo -> pomozna (vstavi a drevo_acc) ostalo
    in
    pomozna Prazno sez

let rec velikost_multimnozice drevo = match drevo with
    | Prazno -> 0
    | Vozlisce (levo, a, st, desno) -> st + velikost_multimnozice levo + velikost_multimnozice desno

let rec seznam_iz_multimn drevo = match drevo with
    | Prazno -> []
    | Vozlisce (levo,a,st,desno) -> 
        let rec fuja b k = if k <= 0 then [] else b::(fuja b (k-1))
        in
        let sez_ajev = fuja a st in
        seznam_iz_multimn levo @ sez_ajev @ seznam_iz_multimn desno