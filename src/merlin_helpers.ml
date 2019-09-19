open! Import
open Astlib.V4_07

let mknoloc txt =
  String_loc.of_concrete (String_loc {txt; loc = Location.none})

let empty_payload =
  let empty_str = Structure.of_concrete (Structure {a = []}) in
  Payload.of_concrete (PStr {a = empty_str})

let hide_attribute =
  Attribute.of_concrete (Attribute {a = mknoloc "merlin.hide"; b = empty_payload})
let focus_attribute =
  Attribute.of_concrete (Attribute {a = mknoloc "merlin.focus"; b = empty_payload})

let append_attr attr attributes =
  let Attributes {a} = Option.value_exn (Attributes.to_concrete attributes) in
  Attributes.of_concrete (Attributes {a = attr::a})

let append_attr_to_pattern attr p =
  let Pattern {ppat_desc; ppat_attributes; ppat_loc} =
    Option.value_exn (Pattern.to_concrete p)
  in
  Pattern.of_concrete
    (Pattern
       { ppat_desc
       ; ppat_loc
       ; ppat_attributes = append_attr attr ppat_attributes })

let hide_pattern p = append_attr_to_pattern hide_attribute p
let focus_pattern p = append_attr_to_pattern focus_attribute p

let append_attr_to_expr attr e =
  let Expression {pexp_desc; pexp_attributes; pexp_loc} =
    Option.value_exn (Expression.to_concrete e)
  in
  Expression.of_concrete
    (Expression
       { pexp_desc
       ; pexp_loc
       ; pexp_attributes = append_attr attr pexp_attributes })

let hide_expression e = append_attr_to_expr hide_attribute e
let focus_expression e = append_attr_to_expr focus_attribute e

let hide_attribute = Option.value_exn (Astlib.Conversions.attribute_of_ast hide_attribute)
let focus_attribute = Option.value_exn (Astlib.Conversions.attribute_of_ast focus_attribute)

let apply_pat ~f p =
  let p = Astlib.Conversions.pattern_to_ast p in
  Option.value_exn (Astlib.Conversions.pattern_of_ast (f p))

let hide_pattern p = apply_pat ~f:hide_pattern p
let focus_pattern p = apply_pat ~f:focus_pattern p

let apply_exp ~f p =
  let p = Astlib.Conversions.expression_to_ast p in
  Option.value_exn (Astlib.Conversions.expression_of_ast (f p))

let hide_expression p = apply_exp ~f:hide_expression p
let focus_expression p = apply_exp ~f:focus_expression p
