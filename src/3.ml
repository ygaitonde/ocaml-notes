(* list expressions *)
let _ = [ 1; 2; 3; 4; 5 ]
let _ = 1::2::3::4::5::[]
let _ = [1] @ [2;3;4] @ [5]

(* product *)
let rec product lst = match lst with
  | [] -> 1
  | x::xs -> x * product xs

(* concat *)
let rec concat lst = match lst with
  | [] -> ""
  | x::xs -> x ^ concat xs 

(* patterns *)
let first_is_bigred = function [] -> false | h::_ -> h = "bigred"
let two_or_four_elements lst = 
  let rec h acc lst = match lst with
      [] -> acc = 2 || acc = 4 | _::xs -> h (acc + 1) xs
  in h 0 lst

let first_two_equal = function  [] -> false | [_] -> false | f::s::_ -> f = s

(* library *)
let fifth_element lst = if List.length lst < 5 then 0 else List.nth lst 4

let sort_descending lst = List.sort compare lst |> List.rev

(* library puzzle *)
let return_last lst = List.nth lst (List.length lst - 1)

let any_zeroes lst = List.exists (fun x -> x = 0) lst

(* take drop *)
let rec take n lst = match lst with 
  | [] -> []
  | h::tl -> if n = 1 then [h] else h::(take (n-1) tl)

let rec drop n lst = match lst with 
  | [] -> []
  | x::xs -> if n = 0 then lst else drop (n-1) (xs)

let rec take_fast n lst = 
  let rec h acc n lst = if n = 0 then acc 
    else match lst with [] -> acc | x::xs -> h (acc @ [x]) (n-1) xs
  in h [] n lst

(* unimodal *)
let is_unimodal lst = 
  let rec dec = function 
      [] -> true 
    | [_] -> true
    | x1::x2::xs -> if x1 >= x2 then dec (x2::xs) else false
  in let rec inc = function
        [] -> true 
      | [_] -> true
      | x1::x2::xs -> if x1 <= x2 then inc (x2::xs) else dec xs
  in inc lst 

let rec powerset lst =
  match lst with
  | []   -> [[]]
  | h::t -> let s = powerset t in
    s @ (List.map (fun x -> h :: x) s)


(* print int list rec *)
let rec print_int_list = function 
  | [] -> () 
  | h::t -> print_endline (string_of_int h); 
    print_int_list t

(* print int list iter *)
let print_int_list' lst = 
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* student *)
type student = { first_name : string ; last_name : string ; gpa : float }

let _ = { first_name = "John"; last_name = "Smith"; gpa = 4.0 }
let get_name stu = (stu.first_name, stu.last_name)

let create_student first last gpa = { first_name=first; last_name = last; gpa = gpa}


(* pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = {name: string; hp: int; ptype: poketype}

let charizard = {name= "Charizard"; hp= 78; ptype= Fire}
let squirtle = {name="Squirtle"; hp=44; ptype=Water}

(* safe hd and tl *)
let safe_hd = function [] -> None | h::_ -> Some h

let safe_tl = function [] -> None | [h] -> Some [h] | _::tl -> Some tl

(* pokefun *)
let rec max_hp lst = 
  let rec h max lst = 
    match lst with 
    | [] -> max
    | hd::t -> (
        match max with 
        | Some max_p ->
          if hd.hp > max_p.hp then h (Some hd) t else h max t
        | None -> h (Some hd) t)
  in 
  h None lst

(* date before *)
type date = int*int*int
let is_before d1 d2 = 
  let year = function _,_,y -> y in
  let month = function _,m,_ -> m in
  let day = function d,_,_ -> d in

  let y1 = year d1 in 
  let y2 = year d2 in

  if y1 != y2 then y1 < y2
  else 
    let m1 = month d1 in
    let m2 = month d2 in 

    if m1 != m2 then m1 < m2
    else 
      let day1 = day d1 in 
      let day2 = day d2 in 
      day1 < day2

let earliest lst = 
  let rec h e lst = 
    match lst with 
    | [] -> e
    | hd::tl -> (
        match e with
        | Some early -> if is_before hd early then h (Some hd) tl else h e tl
        | None -> h (Some hd) tl
      )
  in
  h None lst


(* assoc list *)

(* insert a binding from key k to value v in association list d *)
let insert k v d = (k,v)::d

(* find the value v to which key k is bound, if any, in the assocation list *)
let rec lookup k = function
  | [] -> None
  | (k',v)::t -> if k=k' then Some v else lookup k t


let asc_lst = [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"

let _ = lookup 2 asc_lst
let _ = lookup 4 asc_lst

(* cards *)

type suit = Spades | Diamonds | Hearts | Clubs

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = { suit: suit; rank: rank}

let _ = { suit=Spades; rank=Ace }

let _ = { suit=Clubs; rank=Queen }

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = match x with 
  | x when x > 0 -> Pos
  | x when x < 0 -> Neg
  | _ -> Zero 

let quadrant : int*int -> quad option = fun (x,y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Pos, Neg -> Some II
  | Neg, Neg -> Some III
  | Neg, Pos -> Some IV
  | _ -> None

let quadrant_when = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x > 0 && y < 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x < 0 && y > 0 -> Some IV
  | _ -> None


(* depth *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let depth tree = 
  let rec h depth = function
    | Leaf -> depth
    | Node (_, l, r) ->
      max (h (depth + 1) l) (h (depth + 1) r)
  in h 0 tree

(* shape *)
let rec same_shape t1 t2 = 
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Node _, Leaf | Leaf, Node _ -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2

(* list max exn *)
let list_max = function 
  | [] -> raise (Failure "list_max")
  | lst -> let rec h max = function
      | [] -> max
      | x::xs -> if x > max then h x xs else h max tl
    in h min_int lst

(* list max exn *)
let list_max_str = function 
  | [] -> "empty" 
  | lst -> let rec h max = function
      | [] -> max
      | x::xs -> if x > max then h x xs else h max tl
    in string_of_int (h min_int lst)

(* is_bst *)
type bst_result = Empty | Node of int * int | Invalid

let rec is_bst_helper = function
  | Leaf -> Empty
  | Node (value, left,right) -> (
      match (is_bst_helper left, is_bst_helper right) with
      | Empty, Empty -> Node (value, value)
      | Empty, Node (l, u) -> if value < l then Node (value, u) else Invalid
      | Node (l, u), Empty -> if value > u then Node (l, value) else Invalid
      | Node (l1, u1), Node (l2, u2) ->
        if value > u1 && value < l2 then Node (l1, u2) else Invalid
      | _, _ -> Invalid )

let is_bst root = match is_bst_helper root with Invalid -> false | _ -> true

(* quadrant poly *)
let sign x = if x > 0 then `Pos else if x < 0 then `Neg else `Zero
let quadrant (x, y) =
  match (sign x, sign y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _, _ -> None
