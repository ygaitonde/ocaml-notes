(* operators *)

let _ = 42 * 10
let _ = 3.14 /. 2.

let rec pow a b = if b = 0 then 1. else a *. pow a (b-1)
let _ = pow 4.2 7

(* equality *)

let _ = 42 = 42
(* true *)
let _ = "hi" = "hi"
(* false *)
let _ = "hi" == "hi"

(* assert *)
let _ = 2110 <> 3110

(* if *)
let _ = if 2 > 1 then 42 else 7

(* double fun *)
let double x = x * 2
let () = assert(double 6 = 12)

(* more fun *)

let cube_float x = x*.x*.x
let get_sign x = if x = 0 then 0 else if x > 0 then 1 else -1
let circle_area r = Float.pi *. pow r 2

(* rms *)
let rms x y = sqrt ((pow x 2 +. pow y 2) /. 2.)

(* date fun *)
let valid_date d m  = 
  if (m="Jan" || m="Mar" || m="May" || m="July" || m="Sep" || m = "Dec") && d<=31 then true
  else if (m = "Apr" || m = "Jun" || m="Aug" || m = "Nov") && d <= 30 then true
  else if m = "Feb" && d <= 28 then true else false

(* fib *)
let rec fib n = if n <= 0 then 0 else if n = 1 then 1 else fib (n-1) + fib (n-2)

(* 
  fib fast 
  Requires: n > 0 
 *)
let fib_fast n =
  let rec h n pp p = if n = 1 then p else h (n-1) p (pp + p) in
  h n 0 1 

(* average *)
let (+/.) a b = (a +. b) /. 2.0 

