
open Printf

open Sexplib.Std

type t =
| None
| HOPOPT
| ICMP
| TCP
| UDP
| IPv6
| ICMPv6
| GRE
(* | Other *)
[@@deriving compare, sexp, bin_io]

(* include Core.Comparable.Make( *)
(*     struct type t = t *)
(* 	   let compare = compare *)
(*     end) *)

let of_string string =
  match string with
  | "hopopt" -> HOPOPT
  | "icmp" -> ICMP
  | "tcp" -> TCP
  | "udp" -> UDP
  | "ipv6" -> IPv6
  | "gre" -> GRE
  | "icmpv6" -> ICMPv6
  | _ -> 
    (* Other string *)
    failwith (sprintf "Transport_protocol. of_string: invalid string: %s" string)

let to_string string =
  match string with
  | None -> ""
  | HOPOPT -> "hopopt"
  | ICMP -> "icmp"
  | TCP -> "tcp"
  | UDP -> "udp"
  | IPv6 -> "ipv6"
  | GRE -> "gre"
  | ICMPv6 -> "icmpv6"
  (* | Other string -> string *)

let of_int int =
  match int with
  | 0 -> HOPOPT
  | 1 -> ICMP
  | 6 -> TCP
  | 17 -> UDP
  | 41 -> IPv6
  | 47 -> GRE
  | 58 -> ICMPv6
  | _ -> 
    (* | _ -> Other "" *)
    failwith (sprintf "Transport_protocol. of_string: invalid int: %d" int)

let to_int t =
  match t with
  | None -> -1
  | HOPOPT -> 0
  | ICMP -> 1
  | TCP -> 6
  | UDP -> 17
  | IPv6 -> 41
  | GRE -> 47
  | ICMPv6 -> 58
  (* | Other string -> -1 *)

let equal t1 t2 = Batteries.Int.equal 0 (compare t1 t2)
