let option_sum opt1 opt2 = match opt1, opt2 with
    | Some a , Some b -> Some (a+b)
    | _ -> None

let twostep_map f g0 g1 a = 
    let (x0, x1) = f a in
    (g0 x0, g1 x1)

let rec function_repeat f sez = match sez with
    | [] -> []
    | x::xs when f x > 0 -> (sez_ponovitev (f x) x) @ function_repeat f xs
    | x::xs -> function_repeat f xs

(*za k ne bomo dali nikoli negativno*)
let rec sez_ponovitev k a = match k with
    | 0 -> []
    | _ -> a :: (sez_ponovitev (k-1) a)

let rec function_repeat_tlec f sez =
    let rec pomozna acc s = match s with
        | [] -> acc
        | x::xs -> 
            if f x > 0 then pomozna (sez_ponovitev (f x) x)::acc xs
            else pomozna acc xs
        in 
        pomozna [] sez

(*
let iterate f pogoj x0 =
    let i = ref x0 in
    while pogoj !i do
        i := (f i)
    done
    i
*)

(*---------------------------*)
type 'a improved_list =
    | Prazen
    | Vozlisce of 'a array * 'a improved_list

let test = 
    Vozlisce([|1;2;20|],
    Vozlisce([|17;19;20;30|],
    Vozlisce([|100|], Prazen))
)

let rec count poseben_list = match poseben_list with
    | Prazen -> 0
    | Vozlisce (tabela, l) -> 1 + count l

let rec count_znake p_list = match p_list with
    | Prazen -> 0
    | Vozlisce (tabela, l) -> (Array.length tabela) + count_znake l

let rec nth i poseben_list = 
    let rec pomozna acc i p_list = match p_list with
        | Prazen -> None
        | Vozlisce (tabela, l) -> 
            if Array.length tabela < acc then Some tabela.(acc)
            else pomozna (acc - Array.length tabela) i l
    in
    pomozna i i poseben_list

let rec update poseben_list ind arg = match poseben_list with
    | Prazen -> Prazen
    | Vozlisce (tabela, list) -> 
        if Array.length tabela > i then 
            Vozlisce (tabela, update list (i - Array.length tabela) arg)
        else
            let arr = Array.copy tabela in
            let _ arr.(ind) <- arg in
            Vozlisce(arr,list)

    