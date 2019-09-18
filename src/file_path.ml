open! Import
open Astlib.V4_07

let get_default_path (loc : Location.t) =
  let fname = loc.loc_start.pos_fname in
  match String.drop_prefix ~prefix:"./" fname with
  | Some fname -> fname
  | None       -> fname
;;

let get_default_path_str str =
  match Option.value_exn (Structure.to_concrete str) with
  | Structure { a = [] } -> ""
  | Structure { a = stri :: _ } ->
    match Option.value_exn (Structure_item.to_concrete stri) with
    | Structure_item { pstr_loc = loc; _ } -> get_default_path loc
;;

let get_default_path_sig sig_ =
  match Option.value_exn (Signature.to_concrete sig_) with
  | Signature { a = [] } -> ""
  | Signature { a = sigi :: _ } ->
    match Option.value_exn (Signature_item.to_concrete sigi) with
    | Signature_item { psig_loc = loc; _ } -> get_default_path loc
;;

let get_default_path_str str =
  get_default_path_str (Astlib.Conversions.structure_to_ast str)

let get_default_path_sig sig_ =
  get_default_path_sig (Astlib.Conversions.signature_to_ast sig_)
