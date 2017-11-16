
let eval_string ?(print_outcome = false)
      ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printer p =
  eval_string (Printf.sprintf "#install_printer %s;;" p)

let () =
  if not (install_printer "Rope.Rope_toploop.printer") then
    Format.eprintf "Problem installing Rope-printer@."
