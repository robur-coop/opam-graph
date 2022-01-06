module Set = OpamPackage.Set

type package = OpamPackage.t

let packages (switch : OpamFile.SwitchExport.t) =
  assert (Set.cardinal switch.selections.sel_pinned = 0);
  assert (Set.cardinal switch.selections.sel_compiler = 0);
  assert (Set.subset switch.selections.sel_roots switch.selections.sel_installed);
  switch.selections.sel_installed

let root (switch : OpamFile.SwitchExport.t) =
  assert (Set.cardinal switch.selections.sel_roots = 1);
  Set.choose switch.selections.sel_roots

module Name_set = OpamPackage.Name.Set

let filtered_formula_to_pkgs (_switch : OpamFile.SwitchExport.t)
    ?(set = Name_set.empty) formula =
  OpamFormula.fold_left (fun acc (name, _) -> Name_set.add name acc) set formula

let opam (switch : OpamFile.SwitchExport.t) pkg_name =
  OpamPackage.Name.Map.find pkg_name switch.overlays

(* TODO depexts *)
(* TODO build / dev packages *)
(* TODO constraints (os = "linux") *)
let direct_dependencies (switch : OpamFile.SwitchExport.t) pkg =
  let opam = opam switch pkg in
  let set = filtered_formula_to_pkgs switch (OpamFile.OPAM.depends opam) in
  filtered_formula_to_pkgs switch ~set (OpamFile.OPAM.depopts opam)

module Name_map = OpamPackage.Name.Map

type graph = {
  nodes : Name_set.t Name_map.t ;
  top : OpamPackage.Name.t ;
}

let add_node graph pkg edges =
  let nodes = Name_map.add pkg edges graph.nodes in
  { graph with nodes }

let pp_graph ppf graph =
  Name_map.iter (fun pkg deps ->
      let top = if pkg = graph.top then "ROOT " else "" in
      Format.fprintf ppf "%s%s: %s@."
        top (OpamPackage.Name.to_string pkg)
        (String.concat ", " (List.map OpamPackage.Name.to_string
                               (Name_set.elements deps))))
    graph.nodes

let dependencies (switch : OpamFile.SwitchExport.t) =
  let root_pkg = root switch in
  let top = root_pkg.OpamPackage.name in
  let graph = { top ; nodes = Name_map.empty } in
  let available = switch.selections.sel_installed in
  let rec find_deps graph work =
    match Name_set.choose_opt work with
    | None -> graph
    | Some x ->
      let deps = direct_dependencies switch x in
      let deps =
        Name_set.filter (fun name ->
            OpamPackage.Set.exists
              (fun pkg -> pkg.OpamPackage.name = name)
              available)
          deps
      in
      let graph = add_node graph x deps in
      let work =
        Name_set.diff
          (Name_set.union (Name_set.remove x work) deps)
          (Name_set.of_list (Name_map.keys graph.nodes))
      in
      find_deps graph work
  in
  find_deps graph (Name_set.singleton top)

module Dot = struct

  type t = Odot.graph

  let of_graph (graph:graph) : t =
    let open Odot in
    let stmt_list =
      Name_map.fold (fun pkg deps acc ->
        let stmt =
          let pkg_id = Double_quoted_id (OpamPackage.Name.to_string pkg) in
          let pkg_point = Edge_node_id (pkg_id, None) in
          let deps_points =
            Name_set.elements deps
            |> List.map (fun p -> 
              let id = Double_quoted_id (OpamPackage.Name.to_string p) in
              Edge_node_id (id, None)
            )
          in
          let edge = pkg_point, deps_points, [] in
          Stmt_edge edge
        in
        stmt :: acc
      ) graph.nodes []
    in
    { strict = false; (*todo test params*)
      kind = Digraph;
      id = None;
      stmt_list }

  let pp ppf dot =
    Format.fprintf ppf "%s" (Odot.string_of_graph dot)

end
