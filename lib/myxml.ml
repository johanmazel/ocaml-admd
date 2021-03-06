
open Printf

type xml = 
| Element of Xmlm.tag * xml list
| PCData of string

exception Not_element of xml
exception Not_pcdata of xml
exception No_attribute of string

let to_string_name name =
  sprintf 
    "%s:%s"
    (fst name)
    (snd name)

let to_string_attribute (attribute : Xmlm.attribute) =
  sprintf
    "%s=\"%s\""
    (to_string_name (fst attribute))
    (snd attribute)

let to_string_tag (tag : Xmlm.tag) =
  sprintf 
    "%s %s"
    (to_string_name (fst tag))
    (* (Utils_batteries.to_string_list *)
    (Xml_utils.to_string_list
       to_string_attribute
       (snd tag)
    )

let to_string_data xml =
  match xml with
  | Element (tag, tree_list) -> raise (Not_pcdata xml)
  | PCData string -> string

let filter_element xml_list =
  List.filter
    (fun element ->
      match element with
      | PCData string -> false
      | Element (tag, _) -> true
    )
    xml_list

(* let rec to_string xml = *)
(*   match xml with *)
(*   | Element (tag, xml_list) ->  *)
(*     sprintf  *)
(*       "%s:\n%s" *)
(*       (to_string_tag tag) *)
(*       (Utils_batteries.to_string_list *)
(* 	 to_string *)
(* 	 xml_list *)
(*       ) *)
(*   | PCData string -> *)
(*     string *)

(* let to_string_xml = to_string *)

let tag xml =
  match xml with
  | Element (tag, tree_list) -> tag
  | PCData string -> raise (Not_element xml)

let tag_name xml =
  snd (fst (tag xml))

let attributes xml =
  match xml with
  | Element (tag, xml_list) -> snd tag
  | PCData string -> raise (Not_element xml)

let children tree =
  match tree with
  | Element (tag, tree_list) -> tree_list
  | PCData string -> assert(false)

let in_tree i = 
  let el tag childs = Element (tag, childs)  in
  let data d = PCData d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t = 
  let frag = function
  | Element (tag, childs) -> `El (tag, childs) 
  | PCData d -> `Data d 
  in
  Xmlm.output_doc_tree frag o t

let to_string 
      ?(ns_prefix = fun _ -> None)
      t 
  =
  let buffer = Buffer.create 0 in
  let xmlm_dest = `Buffer buffer in
  let xmlm_output = 
    Xmlm.make_output
      ~nl: true
      ~indent: (Some 2)
      ~ns_prefix: ns_prefix
      xmlm_dest
  in
  out_tree xmlm_output (None, t);
  let xml_string = Buffer.contents buffer in
  xml_string

(* let rec fold acc f xml = *)
(*   let children = children xml in *)

(*   let second_xml = List.nth children 1 in *)

(* match second_xml with *)
(* | Element () *)
