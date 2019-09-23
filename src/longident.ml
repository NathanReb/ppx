open! Import

module T = struct
  type t = longident =
      Lident of string
    | Ldot of t * string
    | Lapply of t * t

  let compare = compare

  let is_normal_ident_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false

  let is_normal_ident = function
    | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "or" ->
      false
    | string ->
      String.for_all string ~f:is_normal_ident_char

  let short_name string =
    if is_normal_ident string
    then string
    else "( " ^ string ^ " )"

  let rec name = function
    | Lident s -> short_name s
    | Ldot (a, b) -> name a ^ "." ^ short_name b
    | Lapply (a, b) -> Printf.sprintf "%s(%s)" (name a) (name b)
end
include T

let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(_, _) -> invalid_arg "Ppx.Longident.flatten"

let flatten_exn lid = flat [] lid

let last_exn = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> invalid_arg "Ppx.Longident.flatten"

let unflatten ~init l =
  List.fold_left l ~init ~f:(fun acc s -> Ldot (acc, s))

(* for cases without dotted operators (e.g. [parse "A.B.C"]) *)
let parse_simple s =
  match String.split s ~on:'.' with
  | [] -> assert false
  | s :: l -> unflatten ~init:(Lident s) l

(* handle ["A.B.(+.+)"] or ["Vec.(.%.()<-)"] *)
let parse s =
  match String.index s '(' with
  | None -> parse_simple  s
  | Some l -> match String.rindex_opt s ')' with
    | None -> invalid_arg "Ppx.Longident.parse"
    | Some r ->
      if not (Int.equal r (String.length s - 1)) then
        invalid_arg "Ppx.Longident.parse";
      let group = if Int.equal r (l + 1) then "()" else
          String.trim (String.sub s ~pos:(l+1) ~len:(r-l-1)) in
      if Int.equal l 0 then Lident group else
        let before = String.sub s ~pos:0 ~len:(l-1) in
        match String.split before ~on:'.' with
        | [] -> Lident group
        | s :: l -> Ldot(unflatten ~init:(Lident s) l, group)
