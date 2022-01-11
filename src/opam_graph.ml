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

let opam_file (switch : OpamFile.SwitchExport.t) pkg_name =
  OpamPackage.Name.Map.find pkg_name switch.overlays

(* TODO depexts *)
(* TODO build / dev packages *)
(* TODO constraints (os = "linux") *)
let direct_dependencies (switch : OpamFile.SwitchExport.t) pkg =
  let pkg_opam_file = opam_file switch pkg in
  let set = filtered_formula_to_pkgs switch (OpamFile.OPAM.depends pkg_opam_file) in
  filtered_formula_to_pkgs switch ~set (OpamFile.OPAM.depopts pkg_opam_file)

let transitive_dependencies (switch : OpamFile.SwitchExport.t) pkg =
  let available = switch.selections.sel_installed in
  let rec aux pkg seen_pkgs = 
    let opam = opam_file switch pkg in
    let set = filtered_formula_to_pkgs switch (OpamFile.OPAM.depends opam) in
    let set = filtered_formula_to_pkgs switch ~set (OpamFile.OPAM.depopts opam) in
    let transitive_set =
      let filtered_set =
        set
        |> Name_set.filter (fun name ->
          OpamPackage.Set.exists
            (fun pkg -> pkg.OpamPackage.name = name)
            available
          && not (Name_set.mem name seen_pkgs)
        )
      in
      let seen_pkgs = Name_set.union seen_pkgs filtered_set
      in
      filtered_set
      |> Name_set.elements
      |> List.concat_map (fun pkg -> aux pkg seen_pkgs |> Name_set.elements)
      |> Name_set.of_list
    in
    Name_set.union set transitive_set
  in
  aux pkg Name_set.empty

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

let dependencies ?(transitive=false) (switch : OpamFile.SwitchExport.t) =
  let available = switch.selections.sel_installed in
  let root_pkg = root switch in
  let top = root_pkg.OpamPackage.name in
  let graph = { top ; nodes = Name_map.empty } in
  let available = switch.selections.sel_installed in
  let rec find_deps graph work =
    match Name_set.choose_opt work with
    | None -> graph
    | Some x ->
      let deps = match transitive with
        | true -> transitive_dependencies switch x 
        | false -> direct_dependencies switch x
      in
      let deps =
        deps
        |> Name_set.filter (fun name ->
          OpamPackage.Set.exists
            (fun pkg -> pkg.OpamPackage.name = name)
            available
        )
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

(*!Note the first entry is seen as the top node*)
type assoc_graph = (string * (string list)) list
  
module Ui = struct

  let dependencies ?(transitive=true) data : assoc_graph =
    (*> todo can be made more efficient*)
    let all_direct_deps = dependencies data in
    let top = all_direct_deps.top in
    let top_str = OpamPackage.Name.to_string top
    in
    let direct_deps = 
      all_direct_deps.nodes
      |> Name_map.find top
    in
    (*> todo can be made more efficient*)
    let all_transitive_deps =
      dependencies ~transitive data in
    let direct_deps_w_transitive_deps =
      direct_deps
      |> Name_set.elements
      |> List.map (fun direct_dep ->
        let transitive_deps =
          all_transitive_deps.nodes
          |> Name_map.find_opt direct_dep
          |> (function
          | None -> Name_set.empty
          | Some v -> v)
          |> Name_set.elements
          |> List.map OpamPackage.Name.to_string 
        in
        let direct_dep = OpamPackage.Name.to_string direct_dep in
        direct_dep, transitive_deps
      )
    in
    let uniquified_deps =
      direct_deps_w_transitive_deps
      |> List.mapi (fun i (direct_dep, transitive_deps) ->
        let unique_direct_dep = Printf.sprintf "%s_%d" direct_dep i in
        let unique_transitive_deps =
          transitive_deps
          |> List.mapi (fun i' transitive_dep ->
            Printf.sprintf "%s_%d.%d" transitive_dep i i'
          )
        in
        unique_direct_dep, unique_transitive_deps
      )
    in
    let unique_direct_deps = uniquified_deps |> List.map fst
    in
    (top_str, unique_direct_deps) :: uniquified_deps

end

module Render = struct 

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

    let of_assoc (graph:assoc_graph) : t =
      let open Odot in
      let stmt_list =
        graph
        |> List.fold_left (fun acc (pkg, deps) ->
          let stmt =
            let pkg_id = Double_quoted_id pkg in
            let pkg_point = Edge_node_id (pkg_id, None) in
            let deps_points =
              deps
              |> List.map (fun pkg -> 
                let id = Double_quoted_id pkg in
                Edge_node_id (id, None)
              )
            in
            let edge = pkg_point, deps_points, [] in
            Stmt_edge edge
          in
          stmt :: acc
        ) [] 
      in
      { strict = false; (*todo test params*)
        kind = Digraph;
        id = None;
        stmt_list }

    let pp ppf dot =
      Format.fprintf ppf "%s" (Odot.string_of_graph dot)

  end

  module Svg = struct

    type t = Tyxml_svg.doc

    module Svg = Tyxml_svg

    (*> goto
      * change svg width+height to pct again - using vw+vh for development
      * svg width+height shouldn't be here for compatibility with user css?
    *)
    let css = {|

svg {
  width : 100vw;
  height : 100vh;
}
      
|}

    type position = {
      x : float;
      y : float;
    }

    module Unit = struct

      let none size = size, None

    end

    (*goto pass data or preformatted string *)
    let make_title =
      let s = Format.asprintf "foo" in
      Svg.(title (txt s))

    let make_circle ~pos ~radius =
      Svg.[
        circle ~a:[
          a_class ["node_circle"];
          a_cx @@ Unit.none pos.x;
          a_cy @@ Unit.none pos.y;
          a_r @@ Unit.none radius;
        ] []
      ]

    let make_node ~pos ~radius =
      let title = make_title in
      Svg.g
        ~a:[Svg.a_class ["node"]]
        (title :: make_circle ~pos ~radius)

    let of_assoc (graph:assoc_graph) : t =
      match graph with
      | [] -> Tyxml_svg.svg []
      | (top, direct_deps) :: layer2_deps ->
        let top_svg =
          let pos = { x = 0.5; y = 0.5 } in
          let radius = 0.1 in
          make_node ~pos ~radius
        in
        let a = [ Svg.a_viewBox (0., 0., 1., 1.) ] in
        Svg.svg ~a [ top_svg ]

    let pp ppf html =
      Format.fprintf ppf "%a@." (Tyxml_svg.pp ()) html

  end
  
  module Html = struct

    module H = Tyxml_html

    type t = H.doc

    let of_assoc (graph:assoc_graph) : t =
      let svg = Svg.of_assoc graph in
      H.html
        (H.head
            (H.title (H.txt "Dependencies"))
            [H.style [H.Unsafe.data Svg.css]]
        )
        (H.body [ H.svg [ svg ] ])

    let pp ppf html =
      Format.fprintf ppf "%a@." (Tyxml_html.pp ()) html
    
  end
  
end
