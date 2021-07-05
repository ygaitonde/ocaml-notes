(* repeat *)
let rec repeat f n x = if n = 0 then x else repeat f (n-1) (f x)

(* product *)
let product_left = List.fold_left ( *. ) 1.0 

let product_right lst = List.fold_right ( *. ) lst 1.0


(* clip *)
let clip n = 
  if n < 0 then 0 
  else if n > 10 then 10
  else n


let cliplist = List.map (fun x -> clip x)

let rec cliplist'= function 
  | [] -> []
  | x::xs -> clip x :: cliplist' xs

(* sum_cube_odd *)
let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

let sum_cube_odd n = 
  0 -- n
  |> List.filter (fun x -> x mod 2 != 0)
  |> List.map (fun x -> x*x*x)
  |> List.fold_right (fun a b -> a + b)

(* exists *)

let rec exists_rec p lst = 
  match lst with 
  | [] -> false
  | x::xs -> p x || exists_rec p xs

let exists_fold p lst = List.fold_left (fun x y -> x || p y) false lst

let exists_lib = List.exists

(* budget *)

let budget_left b = List.fold_left (-) b

let budget_right b = List.fold_right (fun x y -> b - x )
let budget_left b lst = b - List.fold_right (+) lst 0 

let rec budget_rec b = function
  | [] -> b
  | x::xs -> budget_rec (b-x) xs

(* library uncurried *)
let uncurried_append (l1, l2) = List.append l1 l2
let uncurried_compare (v1, v2) = Char.compare v1 v2
let uncurried_max (v1, v2) = Stdlib.max v1 v2

(* uncurry *)
let uncurry f = function (x, y) -> f x y 
let uncurried_nth     = uncurry List.nth
let uncurried_append'  = uncurry List.append
let uncurried_compare' = uncurry Char.compare
let uncurried_max'     = uncurry max


(* max composition *)

let map_comp f g = List.map (fun x -> f g x) 

(* more list fun *)

let g_3 = List.filter (fun x -> String.length x > 3)

let add_1 = List.map (fun x -> x +. 1.0)

let sep_str sep = List.fold_left (fun x y -> x ^ sep ^ y) ""

(* tree map *)
type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

let rec tree_map f = function
  | Leaf -> Leaf 
  | Node (v, l, r) -> Node(f v, tree_map f l, tree_map f r)

(* assoc list keys *)
let keys lst = lst |> List.split |> fst |> List.sort_uniq compare

(* valid matrix *)
let valid_matrix m = match m with
  | [] -> false
  | x::xs -> List.length x > 0 
             && List.for_all (fun y -> List.length y = List.length x) xs

(* row vector add *)
let add_row_vectors m = List.map2 ( + ) m 

(* matrix add *)
let add_matrices = List.map2 add_row_vectors