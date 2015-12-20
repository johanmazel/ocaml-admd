
open Printf

open Sexplib.Std
open Bin_prot.Std

module type Anomaly_type = sig
  type t
  val of_string : string -> t
  val to_string : t -> string

  val compare : t -> t -> int

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val bin_size_t : t -> int
  val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
end

module type Anomaly_value = sig
  type t
  val of_string : string -> t
  val to_string : t -> string

  val compare : t -> t -> int

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val bin_size_t : t -> int
  val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
end

module type Anomaly_description = sig
  type t
  val of_string : string -> t
  val to_string : t -> string

  val compare : t -> t -> int

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val bin_size_t : t -> int
  val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
end

module Make (Anomaly_type : Anomaly_type) (Anomaly_value : Anomaly_value) (Anomaly_description : Anomaly_description) = struct

  let debug_enabled = ref false

  let set_debug bool = debug_enabled := bool

  let debug fmt =
    Printf.kprintf
      (
        if !debug_enabled then
          (fun s -> Format.printf "%s@." s)
        else
          ignore
      )
      fmt

  module Anomaly = struct

    type t =
      {
        indice : int;

        (* date : Core.Date.t; *)
        (* time : Core.Time.t; *)

        anomaly_type : Anomaly_type.t;
        anomaly_value : Anomaly_value.t;
        anomaly_description_option : Anomaly_description.t option;

        slice_list : Slice.t list;

        (* start_time : Core.Time.t; *)
        (* end_time : Core.Time.t;   *)
        start_time : int;
        end_time : int;
      }
    with compare, sexp, bin_io

    let new_t
        indice

        (* date *)
        (* time *)

        anomaly_type
        anomaly_value
        anomaly_description_option

        slice_list

        start_time
        end_time
      =
      {
        indice;

        (* date; *)
        (* time; *)

        anomaly_type;
        anomaly_value;
        anomaly_description_option;

        slice_list;

        start_time;
        end_time;
      }

    let to_string t =
      sprintf
        "%d %s %s\n%s\n%s\n%d %d"
        t.indice

        (Anomaly_type.to_string t.anomaly_type)
        (Anomaly_value.to_string t.anomaly_value)
        (match  t.anomaly_description_option with
         | None -> "No description"
         | Some anomaly_description -> Anomaly_description.to_string anomaly_description
        )
        (* (Utils_batteries.to_string_list *)
        (Xml_utils.to_string_list
           Slice.to_string
           t.slice_list)

        t.start_time
        t.end_time

    let of_xml
        filter_description_lines

        (* date *)
        (* time *)
        
        indice
        xml
      =
      debug "Anomaly: of_xml: call";

      assert(Myxml.tag_name xml = "anomaly");

      debug
        "Anomaly: of_xml: xml:\n%s\n"
        (Myxml.to_string xml)
      ;
      (* TODO: change use Hashtbl to extract attributes *)
      let attribs_list = Myxml.attributes xml in
      let attributes_array = Array.of_list attribs_list in
      let anomaly_type_tuple = attributes_array.(0) in
      let anomaly_value_tuple = attributes_array.(1) in

      let anomaly_type_string = snd anomaly_type_tuple in
      let anomaly_type = Anomaly_type.of_string anomaly_type_string in
      let anomaly_value_string = snd anomaly_value_tuple in
      let anomaly_value =
        Anomaly_value.of_string
          (* reference_setting_container *)
          (* reference_detector_setting_container           *)
          (* data_for_value_of_string *)
         
          anomaly_value_string
      in

      let children_list = Myxml.children xml in

      (* TODO: Replace all assert by option *)

      debug
        "Anomaly: of_xml: children_list:\n\n%s\n"
        (List_ext.to_string
           ~sep: "\n"
           Myxml.to_string
           children_list
        )
      ;

      debug "Anomaly: of_xml: extracting description_string";
      let anomaly_description_option =
        if filter_description_lines = false then
          (
            let anomaly_description_string_option =
              Xmlm_utils.extract_option_single_pcdata_with_tag_from_xml_list
                "description"
                children_list
            in
            let anomaly_description_option = 
              match anomaly_description_string_option with
              | None -> None
              | Some anomaly_description_string ->
                Some (Anomaly_description.of_string anomaly_description_string)
            in
            anomaly_description_option
          )
        else
          None
      in

      (* print_endline ("anomaly_description_string: " ^
         anomaly_description_string); *)
      (* print_endline ("Anomaly_description:\n" ^ (Anomaly_description.to_string anomaly_description)); *)

      (* assert(false); *)

      debug "Anomaly: of_xml: extracting slice_xml_list";
      let slice_xml_list =
        Batteries.List.filter
          (fun element ->
             match Myxml.tag_name element with
             | "slice" -> true
             | _ -> false
          )
          (Myxml.filter_element children_list)
      in

      (* NOT IN ADMD *)
      assert(List.length slice_xml_list > 0);

      debug "Anomaly: of_xml: extracting from_xml";
      let from_xml =
        Xmlm_utils.extract_single_xml_with_tag_from_xml_list
          "from"
          (Myxml.filter_element children_list)
      in

      debug "Anomaly: of_xml: extracting to_xml";
      let to_xml =
        Xmlm_utils.extract_single_xml_with_tag_from_xml_list
          "to"
          (Myxml.filter_element children_list)
      in

      (* let from_attribs_list = Xml.attribs from_xml in                *)
      (* let from_attributes_array = Array.of_list from_attribs_list in *)
      (* let from_sec_tuple = from_attributes_array.(0) in              *)
      (* let from_usec_tuple = from_attributes_array.(1) in             *)
      (* let from_sec = int_of_string (snd from_sec_tuple) in           *)
      (* let from_usec = int_of_string (snd from_usec_tuple) in         *)

      debug "Anomaly: of_xml: from sec/usec";
      let from_attribs_list = Myxml.attributes from_xml in
      let from_attribute_hashtable =
        List.fold_left
          (fun hashtbl_acc (name , value) ->
             Hashtbl.add
               hashtbl_acc
               name
               (int_of_string value);

             hashtbl_acc
          )
          (Hashtbl.create 0)
          from_attribs_list
      in
      if Hashtbl.mem from_attribute_hashtable ("","sec") = false then
        (
          print_endline
            (sprintf
               "Admd: Anomaly: of_xml: cannot find sec attribute in from:\n%s\n\n%s"
               (* (Utils_batteries.to_string_hashtbl *)
               (Xml_utils.to_string_hashtbl
                  ~to_string_key: (fun (uri, local) -> sprintf "%s:%s" uri local)
                  string_of_int
                  from_attribute_hashtable)
               (Myxml.to_string from_xml)
            );
          assert(false)
        );
      if Hashtbl.mem from_attribute_hashtable ("","usec") = false then
        (
          print_endline
            (sprintf
               "Admd: Anomaly: of_xml: cannot find usec attribute in from:\n%s\n\n%s"
               (* (Utils_batteries.to_string_hashtbl *)
               (Xml_utils.to_string_hashtbl
                  ~to_string_key: (fun (uri, local) -> sprintf "%s:%s" uri local)
                  string_of_int
                  from_attribute_hashtable)
               (Myxml.to_string from_xml)
            );
          assert(false)
        );
      let from_sec = Hashtbl.find from_attribute_hashtable ("","sec") in
      (* let from_usec = Hashtbl.find from_attribute_hashtable ("","usec") in *)

      (* if from_usec <> 0 then *)
      (*         ( *)
      (*           print_endline (sprintf "Anomaly: of_xml: invalid from_usec value: %d" from_usec); *)

      (*           assert(false) *)
      (*         ); *)

      debug "Anomaly: of_xml: to sec/usec";
      let to_attribs_list = Myxml.attributes to_xml in
      let to_attribute_hashtable =
        List.fold_left
          (fun hashtbl_acc (name , value) ->
             Hashtbl.add
               hashtbl_acc
               name
               (int_of_string value);

             hashtbl_acc
          )
          (Hashtbl.create 0)
          to_attribs_list
      in
      assert(Hashtbl.mem to_attribute_hashtable ("","sec"));
      assert(Hashtbl.mem to_attribute_hashtable ("","usec"));
      let to_sec = Hashtbl.find to_attribute_hashtable ("","sec") in
      (* let to_usec = Hashtbl.find to_attribute_hashtable ("","usec") in *)

      (* if to_usec <> 0 then *)
      (*         ( *)
      (*           print_endline (sprintf "Anomaly: of_xml: invalid to_usec value: %d" to_usec); *)

      (*           assert(false) *)
      (*         ); *)

      (* debug *)
      (*         "Anomaly: of_xml: slice_xml_list:\n%s" *)
      (*         (Utils_batteries.to_string_list *)
      (*            ~sep: "\n" *)
      (*            Xml.to_string *)
      (*            slice_xml_list *)
      (*         ); *)

      debug "Anomaly: of_xml: building slice";
      let slice_list =
        Batteries.List.map
          Slice.of_xml
          slice_xml_list
      in

      (* debug *)
      (*         "Anomaly: of_xml: slice_list:\n%s" *)
      (*         (Utils_batteries.to_string_list *)
      (*            ~sep: "\n" *)
      (*            (Slice.to_string To_string_mode.Normal) *)
      (*            slice_list *)
      (*         ); *)

      debug "Anomaly: of_xml: end";

      new_t
        indice
        
        (* date *)
        (* time *)

        anomaly_type
        anomaly_value
        anomaly_description_option

        slice_list

        from_sec
        to_sec

    let to_xml
        (* admd_anomaly_export_mode *)
        (* description_to_string_mode *)
        t
      =
      let slice_xml_list =
        Batteries.List.map
          Slice.to_xml
          t.slice_list
      in

      (* print_endline ("Anomaly_description:\n" ^ (Anomaly_description.to_string t.anomaly_description)); *)

      (* assert(false);                                                                                    *)
      let description_xml =
        Myxml.Element 
          (
            (
              ("","description")
              ,
              [ ]
            )
            , 
            [ 
              (Myxml.PCData 
                 (match  t.anomaly_description_option with
                  | None -> "No description"
                  | Some anomaly_description ->
                    (* Anomaly_description.to_string description_to_string_mode anomaly_description *)
                    Anomaly_description.to_string anomaly_description
                 )
              ) 
            ] 
          ) 
      in

      let from_xml =
        Myxml.Element 
          (
            (
              ("","from") 
              ,
              [ 
                (("","sec") , (string_of_int t.start_time));
                (("","usec") , "0") 
              ] 
            )
            ,
            [ ]
          )
      in 
      let to_xml =
        Myxml.Element
          (
            (
              ("","to")
              ,
              [ 
                (("","sec") , (string_of_int t.end_time)); 
                (("","usec") , "0")
              ] 
            )
            ,
            [ ] ) 
      in

      let anomaly_xml_elements =
        ( [ description_xml ])
        @
        (List.append
           slice_xml_list
           [
             from_xml;
             to_xml;
           ]
        )
      in

      let anomaly_xml =
        Myxml.Element
          (
            (
              ("","anomaly")
              ,
              [
                (("","type") , (Anomaly_type.to_string t.anomaly_type)) ; 
                (("","value") , (Anomaly_value.to_string t.anomaly_value)) 
              ]
            )
            ,
            anomaly_xml_elements
          )
      in

      anomaly_xml


    let match_flow_option
        timestamp_sec_start
        timestamp_usec_start
        timestamp_sec_end
        timestamp_usec_end
        nb_packets
        src_ip_option
        dst_ip_option
        proto_option
        src_port_option
        dst_port_option
        t
      =
      let timestamp_ok = (timestamp_sec_end > t.start_time) && (timestamp_sec_start < t.end_time) in

      (* Note: added for debug of problem with anomaly in 20030101 *)
      (* anomaly type="suspicious" value="1.126000,1.189830,503,0 0 0
         0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" *)
      (* filter src_ip="90.155.212.157" dst_ip="202.159.192.34"
         src_port="80" *)
      (* 1406 packets in trace but only 566 with timetsamp filters =>
         bug with metrics timestamps intialized with 0*)

      (* if timestamp_ok = false then *)
      (*   ( *)
      (*     print_endline *)
      (*       (sprintf *)
      (*          "Admd_functor: match_flow: flow %d.%d to %d.%d" *)
      (*          timestamp_sec_start *)
      (*          timestamp_usec_start *)
      (*          timestamp_sec_end *)
      (*          timestamp_usec_end *)
      (*       ); *)

      (*     print_endline *)
      (*       (sprintf *)
      (*          "Admd_functor: match_flow: anomaly: %d to %d" *)
      (*          t.start_time *)
      (*          t.end_time *)
      (*       ); *)
      (*     (\* assert(false) *\) *)
      (*   ); *)

      let result =
        timestamp_ok
        &&
        Batteries.List.fold_left
          (fun acc slice ->
             acc
             ||
             Slice.match_five_tuple_option
               src_ip_option
               dst_ip_option
               proto_option
               src_port_option
               dst_port_option
               slice
          )
          false
          t.slice_list
      in

      (* debug "Anomaly: match_packet_fields: result: %b" result; *)

      (* debug "Anomaly: match_packet_fields: end";               *)

      result

    (* Note: added for debug of problem with anomaly in 20030101 *)
    (* anomaly type="suspicious" value="1.126000,1.189830,503,0 0 0
       0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" *)
    (* filter src_ip="90.155.212.157" dst_ip="202.159.192.34"
       src_port="80" *)
    (* 1406 packets in trace but only 566 with timetsamp filters =>
       bug with metrics timestamps intialized with 0*)

    (* if timestamp_ok = false then *)
    (*   ( *)
    (*     print_endline *)
    (*       (sprintf *)
    (*          "Admd_functor: match_flow: flow %d.%d to %d.%d" *)
    (*          timestamp_sec_start *)
    (*          timestamp_usec_start *)
    (*          timestamp_sec_end *)
    (*          timestamp_usec_end *)
    (*       ); *)

    (*     print_endline *)
    (*       (sprintf *)
    (*          "Admd_functor: match_flow: anomaly: %d to %d" *)
    (*          t.start_time *)
    (*          t.end_time *)
    (*       ); *)
    (*     (\* assert(false) *\) *)
    (*   ); *)

    let match_flow_data
        timestamp_sec_start
        timestamp_usec_start
        timestamp_sec_end
        timestamp_usec_end
        match_timestamps
        (* nb_packets *)
        src_ip
        dst_ip
        proto
        src_port
        dst_port
        
        slice_list
        start_time
        end_time
      =
      (* let timestamp_ok = (timestamp_sec_end > t.start_time) && (timestamp_sec_start < t.end_time) in *)

      let check_slice slice_list =
        Batteries.List.fold_left
          (fun acc slice ->
             acc
             ||
             (Slice.match_five_tuple
                src_ip
                dst_ip
                proto
                src_port
                dst_port
                slice)
          )
          false
          slice_list
      in

      (* (\* debug *\) *)
      (* (\*   "Anomaly: match_flow: %b" *\) *)
      (* (\*   result; *\) *)

      (* (\* let final_result_if = *\) *)
      (* (\*   if match_timestamps then *\) *)
      (* (\*     timestamp_ok *\) *)
      (* (\*     && *\) *)
      (* (\*       result *\) *)
      (* (\*   else *\) *)
      (* (\*     result *\) *)
      (* (\* in *\) *)

      (* let final_result = *)
      (*   ((not match_timestamps) || timestamp_ok) *)
      (*   && *)
      (*   result *)
      (* in *)

      let final_result =
        if match_timestamps then
          let timestamp_ok = (timestamp_sec_end >= start_time) && (timestamp_sec_start <= end_time) in

          if timestamp_ok then
            check_slice slice_list
          else
            false
        else
          check_slice slice_list
      in

      (* if final_result_if <> final_result then *)
      (*   ( *)
      (*     print_endline *)
      (*       (sprintf *)
      (*          "Problem: (%b || %b) && %b should give %b but give %b" *)
      (*          match_timestamps *)
      (*          timestamp_ok *)
      (*          result *)
      (*          final_result_if *)
      (*          final_result *)
      (*       ); *)

      (*     assert(false) *)
      (*   ); *)

      final_result

    let match_flow 
        timestamp_sec_start
        timestamp_usec_start
        timestamp_sec_end
        timestamp_usec_end
        match_timestamps
        (* nb_packets *)
        src_ip
        dst_ip
        proto
        src_port
        dst_port
        
        t
      =
      match_flow_data
        timestamp_sec_start
        timestamp_usec_start
        timestamp_sec_end
        timestamp_usec_end
        match_timestamps
        (* nb_packets *)
        src_ip
        dst_ip
        proto
        src_port
        dst_port
        
        t.slice_list
        t.start_time
        t.end_time

    (* let to_five_tuple_flow_list t = *)
    (*   List.map *)
    (*     Slice.to_five_tuple_flow *)
    (*         t.slice_list *)

    (* let to_five_tuple_option_list t = *)
    (*   List.map *)
    (*     Slice.to_five_tuple_option *)
    (*         t.slice_list *)

  end

  module Anomaly_container = struct

    type t =
      {
        anomaly_list : Anomaly.t list;
      }
    with compare, sexp, bin_io

    let new_t
        anomaly_list
      =
      {
        anomaly_list = anomaly_list;
      }

    let length t = List.length t.anomaly_list

    let to_string
        t
      =
      Xml_utils.to_string_list
        ~sep: "\n\n"
        Anomaly.to_string
        t.anomaly_list

    let iter
        f
        t
      =
      List.iter
        f
        t.anomaly_list

    let filter
        f
        t
      =
     new_t
      (List.filter
        f
        t.anomaly_list
      )

    let to_list t = t.anomaly_list

    let fold_left
        f
        acc
        t
      =
      List.fold_left
        f
        acc
        t.anomaly_list


  end

  module File = struct

    type t =
      {
        filename : string;

        (* date : Core.Date.t; *)
        (* time : Core.Time.t; *)

        algorithm_option : Algorithm.t option;
        analysis_option : Analysis.t option;
        dataset_option : Dataset.t option;

        (* admd_anomaly_list : Admd_anomaly.t list; *)
        anomaly_container : Anomaly_container.t;
      }
    with compare, bin_io

    type t_list = t list
    with compare, bin_io

    type t_list_tuple = t_list * t_list
    with compare, bin_io

    let new_t
        filename

        (* date *)
        (* time *)

        algorithm_option
        analysis_option
        dataset_option

        anomaly_container
      =
      {
        filename;

        (* date; *)
        (* time; *)

        algorithm_option;
        analysis_option;
        dataset_option;

        anomaly_container;
      }

    let of_filename
        filter_description_lines

        (* date_format_string *)
        (* ?default_hour_minute_second *)
        (* time_format_string *)

        parallelization_mode

        (* reference_detector_container *)
        (* reference_detector_setting_container *)
        (* data_for_value_of_string *)

        file_path
      =
      (
        debug "Admd_file: of_filename: call";

        debug "Admd_file: of_filename: %s" file_path;

        (* let file_name = Filename.basename file_path in *)

        (* debug *)
        (*   "get_date: date_format_string: \"%s\"" *)
        (*   date_format_string; *)

        (* let (year, month, day) =  *)
        (*   Scanf.sscanf *)
        (*     file_name *)
        (*     (Scanf.format_from_string date_format_string "%d%d%d%s") *)
        (*     (fun year month day (string : string) (test : int list) -> (year, month, day)) *)
        (*     []  *)
        (* in *)

        (* let date = *)
        (*   Core.Date.create_exn *)
        (*     year *)
        (*     (Core.Month.of_int_exn month) *)
        (*     day *)
        (* in *)

        (* let year, month, day, hour, minute, second = *)
        (*   Extract_time_from_filename.run *)
        (*     ?default_hour_minute_second *)
        (*     time_format_string *)
        (*     file_path *)
        (* in *)

        (* let ofday = *)
        (*   Core.Ofday.create *)
        (*     ~hr: hour *)
        (*     ~min: minute *)
        (*     ~sec: second *)
        (*     () *)
        (* in *)

        (* let time = *)
        (*   Core.Time.of_date_ofday *)
        (*     ~zone: Core.Zone.local *)
        (*     date *)
        (*     ofday *)
        (* in *)

        debug "Admd_file: of_filename: reading file";

        let file = open_in file_path in
        let xmlm_input = `Channel file in

        let (dtd, x) =
          (
            try
              Myxml.in_tree (Xmlm.make_input xmlm_input)
            with
            | Xmlm.Error ((line, column), error) ->
              (
                print_endline
                  (sprintf
                     "File: of_filename: error during parsing at line %d and column %d of file:\n%s\nerror message: %s"
                     line
                     column
                     file_path
                     (Xmlm.error_message error)
                  );

                failwith("Error during XML parsing");
              )
          )
        in

        close_in file;

        debug "File: of_filename: Xml.parse_string finished";        

        debug "File: of_filename:\n%s" (Myxml.to_string x);        

        let algorithm_option, analysis_option, dataset_option, anomaly_container =
          try(
            let tag = Myxml.tag_name x in

            debug "File: of_filename: tag_name %s" tag;
            debug "File: of_filename: tag:\n%s" (Myxml.to_string_tag (Myxml.tag x));

            debug "File: of_filename: filtering anomaly tags among children of tag";

            let xml_list = Myxml.children x in

            debug "File: of_filename: algorithm_option";
            let algorithm_option =
              match 
                Xmlm_utils.extract_option_single_xml_with_tag_from_xml_list
                  "algorithm"
                  xml_list
              with
              | None -> None
              | Some xml ->  
                let algorithm = Algorithm.of_xml xml in
                Some algorithm
            in

            debug "File: of_filename: analysis_option";
            let analysis_option =
              match 
                Xmlm_utils.extract_option_single_xml_with_tag_from_xml_list
                  "analysis"
                  xml_list
              with
              | None -> None
              | Some xml ->  
                let analysis = Analysis.of_xml xml in
                Some analysis
            in

            debug "File: of_filename: dataset_option";
            let dataset_option =
              match 
                Xmlm_utils.extract_option_single_xml_with_tag_from_xml_list
                  "dataset"
                  xml_list
              with
              | None -> None
              | Some xml ->  
                let dataset = Dataset.of_xml xml in
                Some dataset
            in

            debug "File: of_filename: anomalies_xml_list";
            let anomalies_xml_list =
              Batteries.List.filter
                (fun element ->
                   (
                     match Myxml.tag_name element with
                     | "anomaly" -> true
                     | _ -> false
                   )
                )
                (Myxml.filter_element xml_list)
            in

            let indice_xml_tuple_list =
              Batteries.List.mapi
                (fun indice xml -> (indice , xml))
                anomalies_xml_list
            in

            debug "File: of_filename: building simple anomalies";

            let simple_mawilab_anomaly_list =
              Batteries.List.map
                (fun (indice , xml) ->
                   Anomaly.of_xml
                     filter_description_lines

                     (* reference_detector_container *)
                     (* reference_detector_setting_container *)
                     (* data_for_value_of_string *)

                     (* date *)
                     (* time *)

                     (* reference_detector_container         *)
                     (* reference_detector_setting_container *)
                     indice
                     xml
                )
                indice_xml_tuple_list
            in

            (*                if Program_parameters.Debug_mode_parameter.get_value processing_debug_mode >= Program_parameters.High_debug then*)
            (*                        print_endline "\n\n";                                                                                             *)

            (*                if Program_parameters.Debug_mode_parameter.get_value processing_debug_mode >= Program_parameters.High_debug then*)
            (*                        print_endline                                                                                                     *)
            (*                                (sprintf                                                                                                    *)
            (*                                                "File: of_filename: list_anomalies tags:\n%s\n"                                        *)
            (*                                                (Batteries.List.sprint                                                                                  *)
            (*                                                                ~first: ""                                                                                          *)
            (*                                                                ~last: ""                                                                                           *)
            (*                                                                ~sep: "\n"                                                                                          *)
            (*                                                                (fun output value -> Batteries.IO.nwrite output (Mawilab_anomaly.to_string value))                  *)
            (*                                                                mawilab_anomaly_list                                                                                *)
            (*                                                )                                                                                                       *)
            (*                                );                                                                                                          *)
            (*                                                                                                                                *)
            (*                if Program_parameters.Debug_mode_parameter.get_value processing_debug_mode >= Program_parameters.High_debug then*)
            (*                        print_endline "\n\n";                                                                                             *)

            let anomaly_container =
              Anomaly_container.new_t
                simple_mawilab_anomaly_list
            in

            (algorithm_option, analysis_option, dataset_option, anomaly_container)
          )
          with
          | Myxml.Not_pcdata xml ->
            (
              print_endline
                (sprintf
                   "File: of_filename: Not_pcdata error during parsing file:\n%s\n\n%s"
                   (* (Core.Date.to_string date) *)
                   file_path
                   (Myxml.to_string xml)
                );

              failwith("Error during XML parsing");
            )
          | Myxml.Not_element xml ->
            (
              print_endline
                (sprintf
                   "File: of_filename: Not_element error during parsing file:\n%s\n\n%s"
                   (* (Core.Date.to_string date) *)
                   file_path
                   (Myxml.to_string xml)
                );

              failwith("Error during XML parsing");
            )
        in        

        (* debug "File: of_filename: GCing"; *)

        (* Gc.full_major (); *)

        debug "File: of_filename: end";

        new_t
          file_path

          (* date *)
          (* time *)

          algorithm_option
          analysis_option
          dataset_option

          anomaly_container
      )

    let to_filename
        (* description_to_string_mode *)
        t
      =
      (
        debug "File: to_filename: call";

        let algorithm_xml_list =
          match t.algorithm_option with
          | None -> []
          | Some algorithm -> [ Algorithm.to_xml algorithm ]
        in

        let analysis_xml_list =
          match t.analysis_option with
          | None -> []
          | Some analysis -> [ Analysis.to_xml analysis ]
        in

        let dataset_xml_list =
          match t.dataset_option with
          | None -> []
          | Some dataset -> [ Dataset.to_xml dataset ]
        in

        let xml_list =
          List.map
            (* (Anomaly.to_xml description_to_string_mode) *)
            Anomaly.to_xml
            (Anomaly_container.to_list t.anomaly_container)
        in

        let admd_annotation_xml = Myxml.Element
            (
              (
                ("", "admd:annotation")
                ,
                [ 
                  (("","xmlns:admd") , "http://www.nict.go.jp/admd") ;
                  (("","xmlns:xsi") , "http://www.w3.org/2001/XMLSchema-instance") ; 
                  (("","xsi:schemaLocation") , "http://www.nict.go.jp/admd admd.xsd") ]
              )
              ,
              (List.append (List.flatten [ algorithm_xml_list ; analysis_xml_list ; dataset_xml_list ]) xml_list)
            )
        in        

        (* let xml_string = Xml.to_string_fmt admd_annotation_xml in *)
        (* let xml_string = Myxml.to_string admd_annotation_xml in *)

        debug "File: to_filename: building string";
        let xml_string = 
          Myxml.to_string
            ~ns_prefix: (fun _ -> Some "")
            admd_annotation_xml
        in

        (* let xml_string_split_list = Str.split (Str.regexp "\n") xml_string in *)

        (* debug *)
        (*   "to_filename: last line in xml_string: %s" *)
        (*   (List.hd (List.rev xml_string_split_list)); *)

        let file = open_out t.filename in

        (* let final_xml_string = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>\n" ^ xml_string in *)
        let final_xml_string = xml_string in

        debug "File: to_filename: writing string";
        output_string file final_xml_string;
        flush file;

        close_out file;

        debug "File: to_filename: end";
      )

    let to_string to_string_mode t =
      sprintf
        "%s\n%s\n%s\n%s\n%s"
        t.filename
        (match  t.algorithm_option with
         | None -> "No description"
         | Some algorithm -> Algorithm.to_string algorithm
        )
        (match  t.analysis_option with
         | None -> "No analysis"
         | Some analysis -> Analysis.to_string analysis
        )
        (match  t.dataset_option with
         | None -> "No dataset"
         | Some dataset -> Dataset.to_string dataset
        )
        (Anomaly_container.to_string t.anomaly_container)

  end

end
