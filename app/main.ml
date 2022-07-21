
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

let jump () transitive file output_format =
  let module G = Opam_graph in
  let switch = read_file file in
  let data = OpamFile.SwitchExport.read_from_string switch in
  match output_format with
  | `Text ->
    let graph = G.dependencies ~transitive data in
    Format.printf "%a" G.pp_graph graph
  | `Dot ->
    let graph = G.dependencies ~transitive data in
    let dot = G.Render.Dot.of_graph graph in
    Format.printf "%a" G.Render.Dot.pp dot
  | `Dot_ui ->
    let graph = G.Ui.dependencies ~transitive data in
    let dot = G.Render.Dot.of_assoc graph in
    Format.printf "%a" G.Render.Dot.pp dot
  | `Html ->
    let graph = G.Ui.dependencies ~transitive data in
    let sharing_stats =
      data
      |> G.dependencies ~transitive:false
      |> G.calc_sharing_stats in
    let override_css = "\
      .deps-svg-wrap {\
        background: rgb(60, 60, 87); \
      }\
    "
    in
    let html =
      G.Render.Html.of_assoc
        ~override_css
        ~sharing_stats graph
    in
    Format.printf "%a" G.Render.Html.pp html

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let transitive =
  let doc = "Transitive dependencies" in
  Arg.(value & flag & info [ "transitive" ] ~doc)

let formats = [ "html", `Html ; "dot", `Dot ; "dot-ui", `Dot_ui ; "text", `Text ]

let output_format =
  let doc =
    let formats_str =
      formats |> List.map fst |> String.concat ", "
    in
    Printf.sprintf "Output format. Can be one of: %s." formats_str in
  Arg.(value & opt (Arg.enum formats) `Text & info [ "output-format" ] ~doc)

let file =
  let doc = "The opam switch export to graph" in
  Arg.(required & pos 0 (some file) None & info [ ] ~doc ~docv:"FILE")

let cmd =
  let term = Term.(
    const jump
    $ setup_log
    $ transitive
    $ file
    $ output_format
  ) in
  let version = Fmt.str "%d" Opam_graph.visualization_version in
  let info = Cmd.info "opam-graph" ~version in
  Cmd.v info term

let () = Cmd.eval cmd |> exit
