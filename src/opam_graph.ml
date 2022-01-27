
let sprintf = Printf.sprintf

module OSet = OpamPackage.Set

type package = OpamPackage.t

let packages (switch : OpamFile.SwitchExport.t) =
  assert (OSet.cardinal switch.selections.sel_pinned = 0);
  assert (OSet.cardinal switch.selections.sel_compiler = 0);
  assert (OSet.subset switch.selections.sel_roots switch.selections.sel_installed);
  switch.selections.sel_installed

let root (switch : OpamFile.SwitchExport.t) =
  assert (OSet.cardinal switch.selections.sel_roots = 1);
  OSet.choose switch.selections.sel_roots

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

(*!Note the first entry is seen as the root node*)
type assoc_node = {
  name : string;
  uniqueness_postfix : string;
}
type assoc_graph = (assoc_node * (assoc_node list)) list

module Ui = struct

  let dependencies ?(transitive=true) data : assoc_graph =
    (*> todo can be made more efficient*)
    let all_direct_deps = dependencies data in
    let root = all_direct_deps.top in
    let root_str = OpamPackage.Name.to_string root
    in
    let direct_deps =
      all_direct_deps.nodes
      |> Name_map.find root
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
        let direct_dep_node = {
          name = direct_dep;
          uniqueness_postfix = sprintf "%d" i;
        }
        and unique_transitive_deps =
          transitive_deps
          |> List.mapi (fun i' transitive_dep ->
            let uniqueness_postfix = sprintf "%d.%d" i i' in
            { name = transitive_dep; uniqueness_postfix }
          )
        in
        direct_dep_node, unique_transitive_deps
      )
    in
    let unique_direct_deps = uniquified_deps |> List.map fst in
    let root_node = { name = root_str; uniqueness_postfix = "" }
    in
    (root_node, unique_direct_deps) :: uniquified_deps

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

    let id_of_assoc_node node =
      sprintf "%s_%s" node.name node.uniqueness_postfix

    let of_assoc (graph:assoc_graph) : t =
      let open Odot in
      let stmt_list =
        graph
        |> List.fold_left (fun acc (pkg, deps) ->
          let stmt =
            let pkg_id = Double_quoted_id (id_of_assoc_node pkg) in
            let pkg_point = Edge_node_id (pkg_id, None) in
            let deps_points =
              deps
              |> List.map (fun pkg ->
                let id = Double_quoted_id (id_of_assoc_node pkg) in
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
    type 'a elt = 'a Tyxml_svg.elt
    type 'a attr = 'a Tyxml_svg.attrib

    type ('a, 'b) output = {
      svg_content : 'a elt list;
      svg_attr : 'b attr list;
      css : string;
    }

    module Svg = Tyxml_svg

    (*> goto
      * change svg width+height to pct again - using vw+vh for development
      * svg width+height shouldn't be here for compatibility with user css?
    *)
    let initial_css = {|

svg {
  background : slategrey;
}

line {
  stroke-width: 0.004;
  stroke: bisque;
}

.layer2_deps.bg {
  fill: url(#gradient_01);
}

.direct_dep.node:hover {
  transform: scale(2);
}

.layer2_dep.node:hover {
  transform: scale(1.4);
}

|}

    (* disabled CSS
svg {
  width : 100vw;
  height : 100vh;
}

.layer2_deps.bg {
  fill: bisque;
}

.root:hover ~ .node {
  transform: scale(1.1);
}

  transform-origin: center;
  stroke-width: 0.009 !important;
    *)

    (*< Note the '.layer2_deps.bg' selector...
      https://steveliles.github.io/a_multi_class_union_css_selector.html*)
    (* .layer2_deps.bg fills:
       OCaml: a_fill @@ `Color ("url(#gradient_01)", None);
       CSS: fill: url(#gradient_01);
    *)

    let merge_css cs = String.concat "\n" cs

    type position = {
      x : float;
      y : float;
    }

    let center = { x = 0.5; y = 0.5 }
    let root_radius = 0.015

    module Unit = struct

      let none size = size, None

    end

    let make_title ~text =
      let s = sprintf "%s" text in
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

    let make_square ~center_pos ~width =
      let open Gg in
      let center_pos = V2.v center_pos.x center_pos.y in
      let diagonal = V2.v width width in
      let center_displacement = V2.half diagonal in
      let pos = V2.(center_pos - center_displacement) in
      Svg.[
        rect ~a:[
          (* a_stroke @@ `Color ("black", None);
           * a_stroke_width @@ Unit.none 0.001; *)
          a_fill @@ `Color ("rgba(0,0,0,0)", None); (*goto control with css*)
          a_x @@ Unit.none @@ V2.x pos;
          a_y @@ Unit.none @@ V2.y pos;
          a_width @@ Unit.none width;
          a_height @@ Unit.none width;
        ] []
      ]

    let make_node ~pos ~radius ~text ~classes =
      let title = make_title ~text in
      (*> todo; why is this not in Tyxml - browser support missing?*)
      let a_transform_origin = Svg.Unsafe.string_attrib "transform-origin" in
      let a = Svg.[
        a_class ("node" :: classes);
        a_transform_origin @@ sprintf "%f %f" pos.x pos.y;
      ] in
      Svg.g ~a (title :: make_circle ~pos ~radius)

    let make_line ~pos0 ~pos1 =
      Svg.(line ~a:[
        a_x1 @@ Unit.none pos0.x;
        a_y1 @@ Unit.none pos0.y;
        a_x2 @@ Unit.none pos1.x;
        a_y2 @@ Unit.none pos1.y;
      ]) []

    let make_edge ~pos0 ~pos1 ~text ~classes =
      let a = [ Svg.a_class ("edge" :: classes) ] in
      let title = make_title ~text in
      Svg.g ~a [ title; make_line ~pos0 ~pos1 ]

    let make_direct_dep_text dep ~layer2_deps =
      sprintf "Direct dependency: %s\nDirect dependencies: %d"
        dep.name (List.length layer2_deps)

    let make_direct_deps_nodes ~deps_w_positions =
      deps_w_positions |> List.map (fun ((dep, pos), layer2_deps) ->
        let radius = root_radius *. 0.7
        and text = make_direct_dep_text dep ~layer2_deps
        and classes = [ dep.name; "direct_dep" ] in
        make_node ~pos ~radius ~text ~classes
      )

    let make_direct_dep_edge_css dep = sprintf {|
.direct_dep.edge.%s:hover ~ .direct_dep.node.%s {
  transform: scale(2);
}
.direct_dep.edge.%s:hover ~ .layer2_deps.bg.%s {
  fill: dimgrey;
}
.direct_dep.node.%s:hover ~ .layer2_deps.bg.%s {
  fill: dimgrey;
}
|} dep dep dep dep dep dep
    (*< goto move generation of node-css to some other place*)

    (*disabled css:
(*> problem: selected other following children as well*)
.layer2_dep.hitbox.%s:hover ~ .layer2_dep.node.%s {
  transform: scale(1.4);
}

*)

    let make_triangle ~top ~left ~right =
      let a = Svg.[
        a_points [
          top.x, top.y;
          left.x, left.y;
          right.x, right.y;
        ];
        a_fill @@ `Color ("rgba(0,0,0,0)", None); (*goto control with css*)
        (* a_stroke @@ `Color ("black", None);
         * a_stroke_width @@ Unit.none 0.002; *)
      ] in
      Svg.polygon ~a []

    let make_hitbox_direct_dep_edge ~pos0 ~pos1 ~n_edges ~text ~classes =
      let a = [ Svg.a_class ("edge" :: "hitbox" :: classes) ] in
      let title = make_title ~text in
      let left, right =
        let open Gg in
        let n_edges = float (max 1 n_edges) in
        let diff_angle = Float.two_pi /. n_edges in
        let _pos0, pos1 = V2.(v pos0.x pos0.y, v pos1.x pos1.y) in
        let center = V2.v center.x center.y in
        let pos1_rel = V2.(pos1 - center) in
        let height_triangle =
          let radius_pos1_rel, _ = V2.(to_polar pos1_rel |> to_tuple) in
          radius_pos1_rel *. 1.2 in
        let bottom_width =
          if diff_angle < Float.pi_div_2 then
            sin diff_angle *. height_triangle (*Note: Scaling sin by ~ circle-radius*)
          else
            height_triangle
        in
        let normal_pos1 = V2.(ortho pos1_rel / norm pos1_rel) in
        let normal'_pos1 = V2.(-1. * normal_pos1) in
        let right_leg = V2.((0.5 *. bottom_width) * normal_pos1) in
        let left_leg = V2.((0.5 *. bottom_width) * normal'_pos1) in
        let pos1_unit = V2.(pos1_rel / norm pos1_rel) in
        let pos1_extended = V2.(height_triangle * pos1_unit + center) in
        let right = V2.(right_leg + pos1_extended)
        and left  = V2.(left_leg + pos1_extended) in
        { x = V2.x left; y = V2.y left },
        { x = V2.x right; y = V2.y right }
      in
      Svg.g ~a [ title; make_triangle ~top:pos0 ~left ~right ]

    let make_direct_deps_edges ~deps_w_positions =
      let open Gg in
      let n_edges = deps_w_positions |> List.length in
      let svg =
        deps_w_positions |> List.concat_map (fun ((dep, pos), layer2_deps) ->
          let pos1 = pos in
          let pos0, _pos0_angle =
            (*> Note: Need this because of mix of CSS selectors and SVG paint order*)
            let center = V2.v center.x center.y in
            let pos1 = V2.v pos1.x pos1.y in
            let pos1_rel = V2.(pos1 - center) in
            let pos1_rel_angle = V2.angle pos1_rel in
            let pos0_rel = V2.(v root_radius pos1_rel_angle |> of_polar) in
            let pos0 = V2.(pos0_rel + center) in
            { x = V2.x pos0; y = V2.y pos0 }, pos1_rel_angle
          in
          let text = make_direct_dep_text dep ~layer2_deps in
          let classes = [ dep.name; "direct_dep" ] in
          let visual_svg = make_edge ~pos0 ~pos1 ~text ~classes in
          let hitbox_svg =
            make_hitbox_direct_dep_edge
              ~pos0 ~pos1 ~n_edges ~text ~classes
          in
          [ visual_svg; hitbox_svg ]
        )
      in
      let css =
        deps_w_positions
        |> List.fold_left (fun acc_css ((dep, _), _) ->
          let css = make_direct_dep_edge_css dep.name in
          merge_css [ acc_css; css ]
        ) ""
      in
      svg, css

    let make_layer2_nodes_spiral ~layer2_deps ~layer2_deps_center =
      let open Gg in
      let dot_radius = root_radius *. 0.25 in
      layer2_deps |> List.mapi (fun i' layer2_dep ->
        let i' = float i' +. 5.5 in
        let pos_radius = sqrt i' *. 0.007 -. 0.005 in
        let angle_diff = sqrt i' *. Float.two_pi *. 0.005 +. 0.6 in
        let pos_angle = i' *. angle_diff in
        let pos_rel = V2.(v pos_radius pos_angle |> of_polar) in
        let pos = V2.(layer2_deps_center + pos_rel) in
        let pos = { x = V2.x pos; y = V2.y pos } in
        let text = layer2_dep.name in
        let classes = [ layer2_dep.name; "layer2_dep" ] in
        make_node ~pos ~radius:dot_radius ~text ~classes
      )

    (*goto implement as iterative search*)
    let grid_pos ~cell_width i =
      let open Gg in
      let north = V2.v 0. cell_width in
      let west  = V2.ortho north in
      let south = V2.ortho west in
      let east  = V2.ortho south
      in
      let rec aux i' len_side consumed_side dir pos_acc =
        if i = i' then pos_acc else (
          match dir with
          | `East ->
            if consumed_side = len_side then
              aux i' len_side 0 `South pos_acc
            else
              aux (succ i') len_side (succ consumed_side) dir V2.(east + pos_acc)
          | `South ->
            if consumed_side = len_side then
              aux i' len_side 0 `West pos_acc
            else
              aux (succ i') len_side (succ consumed_side) dir V2.(south + pos_acc)
          | `West ->
            if consumed_side = len_side then
              aux i' len_side 0 `North pos_acc
            else
              aux (succ i') len_side (succ consumed_side) dir V2.(west + pos_acc)
          | `North ->
            if consumed_side = len_side then
              (*> Note special case of jumping to next 'level'*)
              aux (succ i') (len_side +2) 1 `East V2.(north + pos_acc)
            else
              aux (succ i') len_side (succ consumed_side) dir V2.(north + pos_acc)
        )
      in
      aux 0 0 0 `East V2.(v 0. 0.)

    let make_hitbox_square ~center_pos ~width ~text ~classes =
      let title = make_title ~text in
      (*> todo; why is this not in Tyxml - browser support missing?*)
      let a_transform_origin = Svg.Unsafe.string_attrib "transform-origin" in
      let a = Svg.[
        a_class ("hitbox" :: classes);
        a_transform_origin @@ sprintf "%f %f" center_pos.x center_pos.y;
      ] in
      Svg.g ~a (title :: make_square ~center_pos ~width)

    let make_layer2_nodes_grid ~layer2_deps ~layer2_deps_center =
      let open Gg in
      let dot_radius = 0.005 in
      let cell_width = dot_radius *. 2.5 in
      layer2_deps |> List.mapi (fun i layer2_dep ->
        let pos = V2.(layer2_deps_center + grid_pos ~cell_width i) in
        let pos = { x = V2.x pos; y = V2.y pos } in
        let text = layer2_dep.name in
        let classes = [ layer2_dep.name; "layer2_dep" ] in
        let visual_svg = make_node ~pos ~radius:dot_radius ~text ~classes in
        let _hitbox_svg =
          make_hitbox_square ~text ~classes
            ~center_pos:pos
            ~width:cell_width in
        [ (* hitbox_svg;  *)visual_svg ]
      )
      |> List.flatten

    let make_layer2_deps ~deps_w_positions =
      let open Gg in
      deps_w_positions |> List.mapi
        (fun i ((dep, direct_dep_pos), layer2_deps) ->
            let layer2_deps_center =
              let direct_dep_pos = V2.(v direct_dep_pos.x direct_dep_pos.y) in
              let center = V2.v center.x center.y in
              let mult = if i mod 2 = 0 then 2.14 else 1.5
              in
              V2.(mult * (direct_dep_pos - center) + center)
            in
            let nodes = make_layer2_nodes_grid ~layer2_deps ~layer2_deps_center in
            let bg =
              let pos =
                { x = V2.x layer2_deps_center; y = V2.y layer2_deps_center }
              and radius = sqrt (float (List.length layer2_deps) +. 1.) *. 0.0115
              and text = ""
              and classes = [ "layer2_deps"; "bg"; dep.name ] in
              make_node ~pos ~radius ~text ~classes
            in
            let edge =
              let pos0 = direct_dep_pos in
              let pos1 =
                { x = V2.x layer2_deps_center; y = V2.y layer2_deps_center } in
              let text = "" in
              let classes = [ "layer2_deps" ] in
              make_edge ~pos0 ~pos1 ~text ~classes
            in
            edge :: bg :: nodes
        )
      |> List.flatten

    let make_shared_deps_css_aux ~dep ~shared_deps =
      shared_deps |> Seq.map (fun shared_dep ->
        sprintf {|
.direct_dep.%s:hover ~ .node.layer2_dep.%s {
  fill: #5454ff;
}
        |} dep.name shared_dep
      )
      |> List.of_seq
      |> merge_css

    let make_shared_deps_css ~deps_w_positions =
      let module SSet = Set.Make(String) in
      let sset_of_deps deps =
        deps
        |> List.map (fun dep -> dep.name)
        |> SSet.of_list
      in
      let layer2_deps_sets =
        deps_w_positions |> List.map (fun (_, layer2_deps) ->
          layer2_deps |> sset_of_deps
        )
      in
      deps_w_positions |> List.map (fun ((dep, _), layer2_deps) ->
        let layer2_deps = sset_of_deps layer2_deps in
        let shared_deps =
          layer2_deps_sets
          |> List.fold_left (fun acc layer2_deps' ->
            SSet.(union acc (inter layer2_deps layer2_deps'))
          ) SSet.empty
          |> SSet.to_seq
        in
        make_shared_deps_css_aux ~dep ~shared_deps
      )
      |> merge_css

    let make_deps (deps:assoc_graph) =
      let deps_w_positions =
        let open Gg in
        let len_deps = List.length deps in
        let diff_angle = Float.two_pi /. float len_deps in
        deps |> List.mapi (fun i (dep, layer2_deps) ->
          let angle = diff_angle *. float i in
          let dist_center = 0.2 in
          let x = cos angle *. dist_center +. center.x
          and y = sin angle *. dist_center +. center.y in
          ((dep, { x; y }), layer2_deps)
        )
      in
      let direct_deps_edges, direct_deps_edges_css =
        make_direct_deps_edges ~deps_w_positions
      and direct_deps_nodes = make_direct_deps_nodes ~deps_w_positions
      and layer2_deps = make_layer2_deps ~deps_w_positions
      and shared_deps_css = make_shared_deps_css ~deps_w_positions
      in
      let svg = direct_deps_edges @ direct_deps_nodes @ layer2_deps in
      let css = merge_css [ direct_deps_edges_css; shared_deps_css ] in
      svg, css

    let svg_defs = Svg.[ defs [
      radialGradient ~a:[
        a_id "gradient_01";
        a_cx @@ Unit.none 0.5;
        a_cy @@ Unit.none 0.5;
        a_r @@ Unit.none 0.5;
      ] [
        stop ~a:[
          a_offset @@ `Percentage 0.;
          a_stop_color "bisque"
        ] [];
        stop ~a:[
          a_offset @@ `Percentage 100.;
          a_stop_color "bisque"; a_stop_opacity 0.
        ] []
      ]
    ]]

    let of_assoc (graph:assoc_graph) : _ output =
      match graph with
      | [] -> { svg_content = []; svg_attr = []; css = "" }
      | (root_pkg, direct_deps) :: layer2_deps ->
        let root_svg =
          let pos = center
          and radius = root_radius
          and text =
            sprintf "%s\nDirect dependencies: %d"
              root_pkg.name (List.length direct_deps)
          and classes = [ root_pkg.name; "root" ] in
          make_node ~pos ~radius ~text ~classes
        in
        let deps_svgs, deps_css = make_deps layer2_deps in
        let svg_attr = [ Svg.a_viewBox (0., 0., 1., 1.) ] in
        let svg_content = svg_defs @ (root_svg :: deps_svgs) in
        let css = merge_css [ initial_css; deps_css ] in
        { svg_content; svg_attr; css }

    let pp ppf html =
      Format.fprintf ppf "%a@." (Tyxml_svg.pp ()) html

  end

  module Html = struct

    module H = Tyxml_html

    type t = H.doc

    let merge_css = String.concat "\n"

    let of_assoc ?(override_css="") (graph:assoc_graph) : t =
      let svg = Svg.of_assoc graph in
      H.html
        (H.head
            (H.title (H.txt "Dependencies"))
            [H.style [H.Unsafe.data  @@ merge_css [ svg.css; override_css ]]]
        )
        (H.body [ H.svg ~a:svg.svg_attr svg.svg_content ])

    let pp ppf html =
      Format.fprintf ppf "%a@." (Tyxml_html.pp ()) html

  end

end
