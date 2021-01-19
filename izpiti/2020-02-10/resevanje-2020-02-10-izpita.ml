let dot_prod (x0,x1,x2) (y0,y1,y2) = x0 * y0 + x1 * y1 + x2 * y2

let fix_second f a b = f b a

let rec combine_and_filter f sez1 sez2 = match sez1, sez2 with
    | [], _ -> []
    | _, [] -> []
    | x::xs, y::ys -> 
        if f x y then (f x y)::combine_and_filter f xs ys
        else combine_and_filter f xs ys

let rec combine_and_filter_tl f sez1 sez2 =
    let rec pomozna acc s1 s2 = match s1, s2 with
        | [], [] -> List.rev acc
        | [], _ -> List.rev acc
        | _, [] -> List.rev acc
        | x::xs, y::ys -> match (f x y) with
            | Some a -> pomozna (a::acc) xs ys
            | None -> pomozna acc xs ys
    in
    pomozna [] sez1 sez2

let rec conditional_print pred sez_nizov = match sez_nizov with
    | [] -> ()
    | [x] -> if pred x then print_string x else ()
    | x::xs -> 
        (if pred x then print_string x else ());
        conditional_print pred xs

(*-------------------------------*)

type ('a, 'b) tree =
    | Prazno
    | AVozlisce of ('a, 'b) tree * 'a * ('a, 'b) tree
    | BVozlisce of ('a, 'b) tree * 'b * ('a, 'b) tree

let test =
    AVozlisce(
        BVozlisce(Prazno, true, Prazno),
        12,
        AVozlisce(
            AVozlisce(Prazno, 0, Prazno),
            5,
            BVozlisce(Prazno, false, Prazno)
        )
    )
let rec adepth drevo = match drevo with 
    | Prazno -> 0
    | AVozlisce (l,a,d) -> 1 + max (adepth l) (adepth d)
    | BVozlisce (l,b,d) -> 
        let ad = max (adepth l) (adepth d) in
        if ad = 0 then 0 else 1 + ad

let rec bdepth drevo = match drevo with 
    | Prazno -> 0
    | BVozlisce (l,a,d) -> 1 + max (bdepth l) (bdepth d)
    | AVozlisce (l,b,d) -> 
        let ad = max (bdepth l) (bdepth d) in
        if ad = 0 then 0 else 1 + ad

type result = {a: int; b: int}

let rec count drevo = 
    let rec pomozna tree acc = match tree with
        | Prazno -> acc
        | AVozlisce(l,_,d) -> {a=acc.a+1; b=acc.b} |> pomozna l |> pomozna d
        | BVozlisce(l,_,d) -> {a=acc.a; b=acc.b+1} |> pomozna l |> pomozna d
    in
    pomozna drevo {a=0; b=0}

let rec is_type_mirror dr1 dr2 = match dr1, dr2 with
    | Prazno, Prazno -> true
    | AVozlisce (l1,a,d1), BVozlisce (l2,b,d2) -> 
        let leva = is_type_mirror l1 l2 in
        let desna = is_type_mirror d1 d2 in
        desna && leva && a = b
    | BVozlisce (l1,a,d1), AVozlisce (l2,b,d2) -> 
        let leva = is_type_mirror l1 l2 in
        let desna = is_type_mirror d1 d2 in
        desna && leva && a = b
    | _, _ -> false

