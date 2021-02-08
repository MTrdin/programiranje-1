(* -------- 1 -------- *)
let rec vsota_stevil sez =
    let rec pomozna acc s = match s with
        | [] -> acc
        | x::xs -> pomozna (x+acc) xs
    in
    pomozna 0 sez
(* -------- 2 -------- *)
let rec urejen_nar sez = match sez with
    | [] -> true
    | [_] -> true
    | x::y::xs -> x <= y && urejen_nar (y::xs)
(* -------- 3 -------- *)
let rec vstavi_celo_st sez a = match sez with
    | [] -> [a]
    | x::xs -> if x < a then x::(vstavi_celo_st xs a) else a::(x::xs)

let rec uredi sez =
    let rec pomozna acc s = match s with
        | [] -> acc
        | x::xs -> pomozna (vstavi_celo_st acc x) xs
    in
    pomozna [] sez
(* -------- 4 -------- *)
let rec vstavi_posebna sez f a = match sez with
    | [] -> [a]
    | x::xs -> if (f x a) then x::(vstavi_posebna xs f a) else a::(x::xs)

let rec uredi_posebna f sez =
    let rec pomozna acc s = match s with
        | [] -> acc
        | x::xs -> pomozna (vstavi_posebna acc f x) xs
    in
    pomozna [] sez

(* -------- 5 -------- *)

type flyer = { status : status ; name : string }

type status =
    | Staff
    | Passenger of prioriteta

type prioriteta =
    | Top
    | Group of int

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)
let prednost potnik1 potnik2 = match potnik1.status, potnik2.status with
    | (Staff, _) -> true
    | Passenger a, Staff -> false
    | Passenger pri1, Passenger pri2 -> 
        match pri1, pri2 with
        | Top, _ -> true
        | Passenger st1, Top -> false
        | Passenger st1, Passenger st2 -> if st1 >= st2 then true else false

let vkrcavanje sez_potnikov = uredi_posebna prednost sez_potnikov
(* -------- 7 -------- *)
let bloki sez_potnikov =
    let urejeni_potniki = vkrcavanje sez_potnikov in

    let rec pomozna acc gradim sez stat = match sez with
    | [] -> gradim::acc
    | potnik::ostali -> 
        if potnik.status = stat then 
            pomozna acc (potnik::gradim) ostali stat
        else
            pomozna (gradim::acc) [potnik] ostali potnik.status
    in

    match urejeni_potniki with
    | [] -> [[]]
    | oseba::ostali -> List.rev (pomozna [] [oseba] urejeni_potniki oseba.status)
    