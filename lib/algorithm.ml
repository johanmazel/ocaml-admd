
open Printf

open Bin_prot.Std

type t =
  {
    name : string;
    version : string;
    description : string;
    url : string;
    parameter : string;
  }
[@@deriving compare, bin_io]

let new_t
    name
    version
    description
    url
    parameter
    =
  {
    name = name;
    version = version ;
    description = description;
    url = url;
    parameter = parameter;
  }

let to_string t =
  sprintf
    "algorithm:\n\tname %s ; version %s ; description %s ;\n\turl %s ; parameter %s"
    t.name
    t.version
    t.description
    t.url
    t.parameter

let of_xml
    xml
    =
  assert(Myxml.tag_name xml = "algorithm");
  
  let attribs_list = Myxml.attributes xml in
  assert(List.length attribs_list = 2);
  let attributes_array = Array.of_list attribs_list in
  let (_ , name) = attributes_array.(0) in
  let (_ , version) = attributes_array.(1) in
  
  let children_list = Myxml.children xml in
  
  (* TODO: Replace all assert by option *)
  
  let description_option =
    Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
      "description"
      children_list
  in
  let description =
    match description_option with
    | None -> "No description"
    | Some description -> description
  in
  
  let url_option =
    Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
      "url"
      children_list
  in
  let url =
    match url_option with
    | None -> "No url"
    | Some url -> url
  in
  
  let parameter_option =
    Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
      "parameter"
      children_list
  in
  let parameter =
    match parameter_option with
    | None -> "No parameter"
    | Some parameter -> parameter
  in
  
  new_t
    name
    version
    description
    url
    parameter

let to_xml
    t
    =
  Myxml.Element
    (
      (
	("","algorithm")
	  ,
	[ (("","name") , t.name) ; (("","version") , t.version) ]
      )
	,
      [
	(Myxml.Element ((("", "description"), [ ]) , [ (Myxml.PCData t.description) ] ))
	;
	(Myxml.Element ((("", "url"), [ ]) , [ (Myxml.PCData t.url) ] ))
      ]
    )
