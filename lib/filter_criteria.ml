
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
  | Src_ip of Ipaddr_sb.t
  | Dst_ip of Ipaddr_sb.t
  | Transport_protocol of Transport_protocol.t
  | Src_port of int
  | Dst_port of int
[@@deriving compare, sexp, bin_io]

let to_string
    t
    =
  match t with
  | Src_ip ipaddr -> "src_addr " ^ (Ipaddr_sb.to_string ipaddr)
  | Dst_ip ipaddr -> "src_addr " ^ (Ipaddr_sb.to_string ipaddr)
  | Transport_protocol protocol -> "proto " ^ (Transport_protocol.to_string protocol)
  | Src_port int -> sprintf "src_port %d" int
  | Dst_port int -> sprintf "dst_port %d" int

let to_xml_data_tuple
    t
    =
  match t with
  | Src_ip ipaddr -> "src_ip", Ipaddr_sb.to_string ipaddr
  | Dst_ip ipaddr -> "dst_ip", Ipaddr_sb.to_string ipaddr
  | Transport_protocol protocol -> "proto" , (Transport_protocol.to_string protocol)
  | Src_port port -> "src_port" , (string_of_int port)
  | Dst_port port -> "dst_port" , (string_of_int port)

let of_string_tuple
    (type_string , data_string)
    =
  match type_string with
  | "src_ip" -> 
    (
      try
        Src_ip (Ipaddr_sb.of_string_exn data_string)
      with
      | Failure string -> 
        failwith 
          (sprintf
             "Filter_criteria: of_string_tuple: %s - %s caused URI_IP to raise %s"
             type_string
             data_string
             string
          )
    )
  | "dst_ip" -> 
    (
      try
	Dst_ip (Ipaddr_sb.of_string_exn data_string)
      with
      | Failure string -> 
        failwith 
          (sprintf
             "Filter_criteria: of_string_tuple: %s - %s caused URI_IP to raise %s"
             type_string
             data_string
             string
          )
    )
  | "proto" -> Transport_protocol (Transport_protocol.of_string data_string)
  | "src_port" -> Src_port (int_of_string data_string)
  | "dst_port" -> Dst_port (int_of_string data_string)
  | string -> 
    print_endline 
      (sprintf
	 "Filter_criteria: of_string_tuple: invalid string: \"%s\""
	 string
      );
    assert(false)

let match_five_tuple_option
    src_ip_option
    dst_ip_option
    proto_option
    src_port_option
    dst_port_option
    t
    =
  match t with
  | Src_ip src_ipaddr_filter ->
    (match src_ip_option with
    | None -> false
    | Some src_ip_to_match -> Ipaddr.compare (Ipaddr_sb.to_ipaddr src_ipaddr_filter) src_ip_to_match = 0
    )
  | Dst_ip dst_ipaddr_filter ->
    (match dst_ip_option with
    | None -> false
    | Some dst_ip_to_match -> Ipaddr.compare (Ipaddr_sb.to_ipaddr dst_ipaddr_filter) dst_ip_to_match = 0
    )
  | Transport_protocol protocol ->
    (match proto_option with
    | None -> false
    | Some proto_to_match -> Transport_protocol.compare protocol proto_to_match = 0
    )
  | Src_port src_port ->
    (match src_port_option with
    | None -> false
    | Some src_port_to_match ->
      (src_port <> src_port_to_match) = false
    )
  | Dst_port dst_port ->
    (match dst_port_option with
    | None -> false
    | Some dst_port_to_match ->
      (dst_port <> dst_port_to_match) = false
    )

let match_five_tuple
      src_addr
      dst_addr
      protocol
      src_port
      dst_port
      t
  =
  match t with
  | Src_ip src_ipaddr_filter ->
     Ipaddr.compare (Ipaddr_sb.to_ipaddr src_ipaddr_filter) src_addr = 0
  | Dst_ip dst_ipaddr_filter ->
     Ipaddr.compare (Ipaddr_sb.to_ipaddr dst_ipaddr_filter) dst_addr = 0
  | Transport_protocol protocol_filter ->
     Batteries.Int.equal 0 (Transport_protocol.compare protocol_filter protocol)
  | Src_port src_port_filter ->
     Batteries.Int.equal src_port_filter src_port
  | Dst_port dst_port_filter ->
     Batteries.Int.equal dst_port_filter dst_port

let to_int
    t
    =
  match t with
  | Src_ip _ -> 0
  | Dst_ip _ -> 1
  | Transport_protocol _ -> 2
  | Src_port _ -> 3
  | Dst_port _ -> 4

