
let to_string_list
    ?first: (first = "")
    ?last: (last = "")
    ?sep: (sep = " ")
    to_string_value
    list
    =
  
  let innerIO_output = Batteries.IO.output_string () in
  
  Batteries.List.print
    ~first: first
    ~last: last
    ~sep: sep
    (fun output value ->
      Batteries.IO.nwrite output
	(to_string_value value)
    )
    innerIO_output
    list;
  
  Batteries.IO.close_out innerIO_output

module Set = struct
  include Set.Make(Transport_protocol)

  let of_list = List.fold_left (fun acc x -> add x acc) empty
  let to_list = elements

  let to_string
      ?sep: (sep = " ")
      t
    =
    let list = elements t in

    to_string_list
      ~sep
      Transport_protocol.to_string
      list
end
