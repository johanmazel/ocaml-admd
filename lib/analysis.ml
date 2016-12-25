
open Printf

open Bin_prot.Std

type t =
  {
    description : string;
 (* datetime : Unix.tm; *)
    datetime : string; 
    analyst : string;
    organization : string;
  }
[@@deriving compare, bin_io]

let new_t
    description
    datetime
    analyst
    organization
    =
  {
    description = description;
    datetime = datetime;
    analyst = analyst;
    organization = organization;
  }

let to_string t =
  (* let time_tm = t.datetime in            *)
  
  (* let time_string =                      *)
  (* 	sprintf "%d-%2d-%2dT%2d:%2d:%2d.000" *)
  (* (1900 + time_tm.Unix.tm_year) *)
  (* 		(time_tm.Unix.tm_mon + 1)          *)
  (* 		time_tm.Unix.tm_mday               *)
  (* 		time_tm.Unix.tm_hour               *)
  (* 		time_tm.Unix.tm_min                *)
  (* 		time_tm.Unix.tm_sec                *)
  (* in                                     *)
  
  (* let time_string_reformated =           *)
  (* 	BatString.map                        *)
  (* 		(fun char ->                       *)
  (* 					(match char with             *)
  (* 						| ' ' -> '0'               *)
  (* 						| _ -> char                *)
  (* 					)                            *)
  (* 		)                                  *)
  (* 		time_string                        *)
  (* in                                     *)
  
  sprintf
    "analysis:\n\tdescription %s ; datetime %s ; analyst %s ; organization %s"
    (* "analysis:\n\tdescription %s ; analyst %s ; organization %s" *)
    t.description
	  (* time_string_reformated *)
    t.datetime
    t.analyst
    t.organization

let of_xml
    xml
    =
  assert(Myxml.tag_name xml = "analysis");
  
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
  
  let datetime_option =
    Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
      "datetime"
      children_list
  in
  let datetime =
    match datetime_option with
    | None -> "No datetime"
    | Some datetime -> datetime
  in
  
  
  let analyst_option =
    Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
      "analyst"
      children_list
  in
  let analyst =
    match analyst_option with
    | None -> "No analyst"
    | Some analyst -> analyst
  in
  
  let organization_option =
    Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
      "organization"
      children_list
  in
  let organization =
    match organization_option with
    | None -> "No organization"
    | Some organization -> organization
  in
  
  new_t
    description
    datetime
    analyst
    organization

let to_xml
    t
    =
  Myxml.Element
    (
      (
	("", "analysis")
	  ,
	[ ]
      )
	,
      [ 
	(Myxml.Element ((("", "description") , [ ]) , [ (Myxml.PCData t.description) ] )) ;
	(* (Xml.Element ("datetime", "") , [ ]) , [ (Xml.PCData time_string_reformated) ] )) ; *)
	(Myxml.Element ((("", "analyst") , [ ]) , [ (Myxml.PCData t.analyst) ] )) ;
	(Myxml.Element ((("", "organization") , [ ]) , [ (Myxml.PCData t.organization) ] ))
      ]
    )
