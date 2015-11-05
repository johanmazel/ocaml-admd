
open Printf

open Bin_prot.Std

type t =
  {
    description : string;
    url : string;
  }
with compare, bin_io

let new_t
    description
    url
    =
  {
    description = description;
    url = url;
  }

let to_string t =
  sprintf
    "dataset:\n\tdescription %s ; url %s"
    t.description
    t.url

let of_xml
    xml
    =
  assert(Myxml.tag_name xml = "dataset");
  
  let children_list = Myxml.children xml in
  
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
  
  new_t
    description
    url

let to_xml
    t
    =
  Myxml.Element
    (
      (
	("", "dataset")
	  ,
	[ ]
      )
	,
      [
	(Myxml.Element ((("", "description") , [ ]) , [ (Myxml.PCData t.description) ] )) ;
	(Myxml.Element ((("", "url") , [ ]) , [ (Myxml.PCData t.url) ] ))
      ]
    )
