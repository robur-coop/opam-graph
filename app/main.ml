
let read_file file =
  try
    let fh = open_in file in
    try
      let content = really_input_string fh (in_channel_length fh) in
      close_in_noerr fh ;
      content
    with _ ->
      close_in_noerr fh;
      invalid_arg ("Error reading file: " ^ file)
  with _ -> invalid_arg ("Error opening file " ^ file)

let () =
  match Sys.argv with
  | [| _ ; output_format; file |] -> (
    let switch = read_file file in
    let data = OpamFile.SwitchExport.read_from_string switch in
    let graph = Opam_graph.dependencies data in
    match output_format with
    | "text" -> Format.printf "%a" Opam_graph.pp_graph graph
    | "dot" ->
      let dot = Opam_graph.Dot.of_graph graph in
      Format.printf "%a" Opam_graph.Dot.pp dot
    | _ -> failwith "Unsupported output format"
  )
  | _ ->
    print_endline "expecting exactly one argument";
    exit 1

