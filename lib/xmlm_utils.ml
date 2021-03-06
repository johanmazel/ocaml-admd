
open Printf

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
	(fun s -> Format.printf "[Xmlm_utils]: %s@." s)
      else
	ignore
    )
    fmt

let extract_attribute_hashtable_from_xml
    (xml : Myxml.xml)
    =
  let attribs_list : Xmlm.attribute list = Myxml.attributes xml in
  
  let hastable =
    List.fold_left
      (fun hashtable_acc (name_tuple , value_string) ->
	Hashtbl.add
	  hashtable_acc
	  name_tuple
	  value_string;
	
	hashtable_acc
      )
      (Hashtbl.create 0)
      attribs_list
  in
  
  hastable

let extract_single_xml_with_tag_from_xml_list
    tag_name
    (xml_list : Myxml.xml list)
    =
  (* debug                                                                         *)
  (* 	"extract_single_xml_with_tag_from_xml_list: call"                           *)
  (* ;                                                                             *)
  
  (* debug                                                                         *)
  (* 	"extract_single_xml_with_tag_from_xml_list: looking for tag %s in:\n\n%s\n" *)
  (* 	tag_name                                                                    *)
  (* 	(Utils_batteries.to_string_list                                             *)
  (* 			~sep: "\n"                                                              *)
  (* 			Xml.to_string                                                           *)
  (* 			xml_list                                                                *)
  (* 	)                                                                           *)
  (* ;                                                                             *)
  
  let xml_list_filtered =
    Batteries.List.filter
      (fun element ->
	  (* debug                                                 *)
	  (* 	"extract_single_xml_with_tag_from_xml_list: tag %s" *)
	  (* 	(Xml.tag element)                                   *)
	  (* ;                                                     *)
	
	try
	  (
	    let tag_name_element = Myxml.tag_name element in

	    if String.compare tag_name_element tag_name = 0 then
	      true
	    else
	      false
	  )
	with
	| Myxml.Not_element xml ->
	  false
      )
      xml_list
  in
  
  (* debug                                                                     *)
  (* 	"extract_single_xml_with_tag_from_xml_list: xml_list_filtered:\n\n%s\n" *)
  (* 	(Utils_batteries.to_string_list                                         *)
  (* 			~sep: "\n"                                                          *)
  (* 			Xml.to_string                                                       *)
  (* 			xml_list_filtered                                                   *)
  (* 	)                                                                       *)
  (* ;                                                                         *)
  
  if List.length xml_list_filtered <> 1 then
    (
      print_endline
	(sprintf
	   "Xml_light_utils: extract_single_xml_with_tag_from_xml_list: found %d xml for tag \"%s\";\nxml_list:\n%s\nxml_list_filtered:\n%s\n"
           (List.length xml_list_filtered)
	   tag_name
	   (* (Utils_batteries.to_string_list *)
	   (Xml_utils.to_string_list
	      ~sep: "\n"
	      Myxml.to_string
	      xml_list
	   )
	   (* (Utils_batteries.to_string_list *)
	   (Xml_utils.to_string_list
	      ~sep: "\n"
	      Myxml.to_string
	      xml_list_filtered
	   )
	);
      
      assert(false);
    );
  
  let xml = List.hd xml_list_filtered in
  
  (* debug                                              *)
  (* 	"extract_single_xml_with_tag_from_xml_list: end" *)
  (* ;                                                  *)
  
  xml

let extract_option_single_xml_with_tag_from_xml_list
    tag_name
    (xml_list : Myxml.xml list)
  =
  (
    (* debug "extract_single_xml_with_tag_from_xml_list: call"; *)

    (* debug                                                                         *)
    (* 	"extract_single_xml_with_tag_from_xml_list: looking for tag %s in:\n\n%s\n" *)
    (* 	tag_name                                                                    *)
    (* 	(Utils_batteries.to_string_list                                             *)
    (* 			~sep: "\n"                                                              *)
    (* 			Xml.to_string                                                           *)
    (* 			xml_list                                                                *)
    (* 	)                                                                           *)
    (* ;                                                                             *)

    let xml_list_filtered =
      Batteries.List.filter
        (fun element ->
	   (* debug                                                 *)
	   (* 	"extract_single_xml_with_tag_from_xml_list: tag %s" *)
	   (* 	(Xml.tag element)                                   *)
	   (* ;                                                     *)

	   try
	     (
	       let tag_name_element = Myxml.tag_name element in

	       if String.compare tag_name_element tag_name = 0 then
		 true
	       else
		 false
	     )
	   with
	   | Myxml.Not_element xml ->
	     false
        )
        xml_list
    in

    (* debug *)
    (*   "extract_single_xml_with_tag_from_xml_list: xml_list_filtered:\n\n%s\n" *)
    (*   (Utils_batteries.to_string_list *)
    (* 	 ~sep: "\n" *)
    (* 	 Myxml.to_string *)
    (* 	 xml_list_filtered *)
    (*   ); *)

    match List.length xml_list_filtered with
    | 0 -> None
    | 1 -> 
      let xml = List.hd xml_list_filtered in

      (* debug "extract_single_xml_with_tag_from_xml_list: end"; *)

      Some xml
    | _ ->
      (
        print_endline
	  (sprintf
	     "Xml_light_utils: extract_option_xml_with_tag_from_xml_list: %d xml(s) for tag \"%s\";\nxml_list:\n%s\nxml_list_filtered:\n%s\n"
             (List.length xml_list_filtered)
	     tag_name
             (* (Utils_batteries.to_string_list *)
             (Xml_utils.to_string_list
	        ~sep: "\n"
	        Myxml.to_string
	        xml_list
	     )
             (* (Utils_batteries.to_string_list *)
             (Xml_utils.to_string_list
	        ~sep: "\n"
	        Myxml.to_string
	        xml_list_filtered
	     )
	  );

        assert(false);
      )
  )

let extract_single_pcdata_with_tag_from_xml_list
    tag_name
    xml_list
  =
  (* debug "extract_single_pcdata_with_tag_from_xml_list: call"; *)

  let xml =
    extract_single_xml_with_tag_from_xml_list
      tag_name
      xml_list
  in

  let children_xml_list = Myxml.children xml in

  (* debug *)
  (*   "extract_single_pcdata_with_tag_from_xml_list: children_xml_list length for %s: %d:\n\n%s\n\n%s\n" *)
  (*   tag_name *)
  (*   (List.length children_xml_list) *)
  (*   (Myxml.to_string xml) *)
  (*   (Utils_batteries.to_string_list *)
  (*      ~sep: "\n" *)
  (*      Myxml.to_string *)
  (*      children_xml_list *)
  (*   ) *)
  (* ; *)

  let string =
    match List.length children_xml_list with
    | 0 -> ""
    | 1 ->
      let children_xml = List.hd children_xml_list in
      let pcdata =
	match children_xml with
	| Myxml.Element (tag , xml_list_) -> assert(false)
	| Myxml.PCData string -> string
      in

      pcdata
    | _ ->
      print_endline
	(sprintf
	   "Xml_light_utils: extract_single_pcdata_with_tag_from_xml_list: children_xml_list lentgh for %s: %d:\n\n%s\n\n%s\n"
	   tag_name
	   (List.length children_xml_list)
	   (Myxml.to_string xml)
           (* (Utils_batteries.to_string_list *)
           (Xml_utils.to_string_list
	      ~sep: "\n"
	      Myxml.to_string
	      children_xml_list
	   )
	);

      assert(false);
  in

  (* debug "extract_single_pcdata_with_tag_from_xml_list: end"; *)

  string



let extract_option_single_pcdata_with_tag_from_xml_list
    tag_name
    xml_list
  =
  (* debug "extract_single_pcdata_with_tag_from_xml_list: call"; *)

  match 
    extract_option_single_xml_with_tag_from_xml_list
      tag_name
      xml_list
  with
  | None -> None
  | Some xml ->  
    let children_xml_list = Myxml.children xml in

    debug
      "extract_single_pcdata_with_tag_from_xml_list: children_xml_list length for %s: %d:\n\n%s\n\n%s\n"
      tag_name
      (List.length children_xml_list)
      (Myxml.to_string xml)
      (* (Utils_batteries.to_string_list *)
      (Xml_utils.to_string_list
         ~sep: "\n"
         Myxml.to_string
         children_xml_list
      )
    ;

    let string =
      match List.length children_xml_list with
      | 0 -> ""
      | 1 ->
        let children_xml = List.hd children_xml_list in
        let pcdata =
	  match children_xml with
	  | Myxml.Element (tag, xml_list_) -> assert(false)
	  | Myxml.PCData string -> string
        in

        pcdata
      | _ ->
        print_endline
	  (sprintf
	     "Xml_light_utils: extract_single_pcdata_with_tag_from_xml_list: children_xml_list lentgh for %s: %d:\n\n%s\n\n%s\n"
	     tag_name
	     (List.length children_xml_list)
	     (Myxml.to_string xml)
	     (* (Utils_batteries.to_string_list *)
             (Xml_utils.to_string_list
	        ~sep: "\n"
	        Myxml.to_string
	        children_xml_list
	     )
	  );

        assert(false);
    in

    (* debug "extract_single_pcdata_with_tag_from_xml_list: end"; *)

    Some string
