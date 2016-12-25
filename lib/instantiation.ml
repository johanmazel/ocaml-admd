
open Sexplib.Std
open Bin_prot.Std

module Base_type = struct
    
  type t = string [@@deriving compare, sexp, bin_io]

  let of_string string = string
  let to_string t = t
    
end

module Base_value = struct
    
  type t = string [@@deriving compare, sexp, bin_io]
  let of_string string = string
  let to_string t = t
    
end

module Base_description = struct
    
  type t = string [@@deriving compare, sexp, bin_io]
  let bin_size_t_ = bin_size_t
  let of_string string = string
  let to_string t = t

end

module Base = Data.Make(Base_type)(Base_value)(Base_description)

