
open Sexplib.Std
open Bin_prot.Std

type t =
  | V4 of int32
  | V6 of int32 * int32 * int32 * int32
with sexp, bin_io

let (&&&) x y = Int32.logand x y
let (>|>) x y = Int32.shift_right_logical x y

let compare_v4 a b =
  let c = Int32.compare (a >|> 1) (b >|> 1) in
  if c = 0 then Int32.compare (a &&& 1l) (b &&& 1l) else c

let compare t1 t2 =
  match t1,t2 with
  | V4 a, V4 b ->
    compare_v4 a b
  | V6 (a1,b1,c1,d1), V6 (a2,b2,c2,d2) ->
    (match compare_v4 a1 a2 with
     | 0 -> begin
         match compare_v4 b1 b2 with
         | 0 -> begin
             match compare_v4 c1 c2 with
             | 0 -> compare_v4 d1 d2
             | n -> n end
         | n -> n end
     | n -> n
    )
  | V4 _, V6 _ -> raise (Invalid_argument "Ipaddr_sb. inter: IPv4 and IPv6")
  | V6 _, V4 _ -> raise (Invalid_argument "Ipaddr_sb. inter: IPv6 and IPv4")

let of_ipaddrv4 ipaddrv4 = V4 (Ipaddr.V4.to_int32 ipaddrv4)
let of_ipaddrv6 ipaddrv6 = 
    let a, b, c, d = Ipaddr.V6.to_int32 ipaddrv6 in
    V6 (a ,b ,c, d)

let of_ipaddr ipaddr =
  match ipaddr with
  | Ipaddr.V4 ipaddrv4 -> of_ipaddrv4 ipaddrv4
  | Ipaddr.V6 ipaddrv6 -> of_ipaddrv6 ipaddrv6
                       
let to_ipaddr t =
  match t with
  | V4 int32 -> Ipaddr.V4 (Ipaddr.V4.of_int32 int32)
  | V6 (a, b, c, d) ->
    Ipaddr.V6 (Ipaddr.V6.of_int32 (a, b, c, d))
 
let to_string t =
  Ipaddr.to_string (to_ipaddr t)

let of_string string = 
  match Ipaddr.of_string string with
  | None -> None
  | Some ipaddr -> Some (of_ipaddr ipaddr)

let of_string_exn string = of_ipaddr (Ipaddr.of_string_exn string)

let is_broadcast t =
  match t with
  | V4 int32 -> int32 = Int32.max_int
  | V6 (a, b, c, d) ->
    (a = Int32.max_int)
    &&
    (b = Int32.max_int)
    &&
    (c = Int32.max_int)
    &&
    (d = Int32.max_int)
  
