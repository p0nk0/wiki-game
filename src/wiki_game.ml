open! Core
module Page = String

module Path = struct
  module T = struct
    type t = Page.t list [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Wiki = struct
  module Connection = struct
    module T = struct
      type t = Page.t * Page.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]
end

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a"
  |> to_list
  |> List.map ~f:(fun elem -> R.attribute "href" elem)
  |> List.filter ~f:(fun elem -> String.is_prefix elem ~prefix:"/wiki/")
  |> List.filter ~f:(fun elem ->
       match Wikipedia_namespace.namespace elem with
       | None -> true
       | _ -> false)
  |> String.Set.of_list
  |> Set.to_list
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module G = Graph.Imperative.Digraph.Concrete (Page)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes _ = [ `Dir `Forward ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let trim_title title =
  Lambda_soup_utilities.get_title title
  |> Str.global_replace (Str.regexp {|\.|}) ""
  |> Str.global_replace (Str.regexp {| - Wikipedia|}) ""
  |> Str.global_replace (Str.regexp {|(|}) ""
  |> Str.global_replace (Str.regexp {|)|}) ""
  |> Str.global_replace (Str.regexp {| |}) "_"
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let start_page = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let edges = ref Wiki.Connection.Set.empty in
  let visited = Page.Hash_set.create () in
  let to_visit = Queue.create () in
  Queue.enqueue to_visit start_page;
  let rec traverse depth =
    match Queue.dequeue to_visit with
    | None -> ()
    | Some current_node ->
      if not (Hash_set.mem visited current_node)
      then Hash_set.add visited current_node;
      if depth > 0
      then
        get_linked_articles current_node
        |> List.filter ~f:(fun next_node ->
             not (Hash_set.mem visited next_node))
        |> List.iter ~f:(fun next_node ->
             edges := Set.add !edges (current_node, next_node);
             Queue.enqueue
               to_visit
               (File_fetcher.fetch_exn how_to_fetch ~resource:next_node));
      traverse (depth - 1)
  in
  traverse max_depth;
  let graph = G.create () in
  Set.iter !edges ~f:(fun (page1, page2) ->
    G.add_edge
      graph
      (trim_title page1)
      (File_fetcher.fetch_exn how_to_fetch ~resource:page2 |> trim_title));
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)

(* my code is mostly copy and pasted from visualize and mashed with maze *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let start_page = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let paths = ref Page.Map.empty in
  let visited = Page.Hash_set.create () in
  let to_visit = Queue.create () in
  Queue.enqueue to_visit start_page;
  let rec bfs depth =
    match Queue.dequeue to_visit with
    | None -> ()
    | Some current_node ->
      if not (Hash_set.mem visited current_node)
      then Hash_set.add visited current_node;
      if depth > 0
      then
        (* print_s [%message "\n\n" (Lambda_soup_utilities.get_title
           current_node : string)]; *)
        get_linked_articles current_node
        |> List.filter ~f:(fun next_node ->
             not (Hash_set.mem visited next_node))
        |> List.iter ~f:(fun next_node ->
             (* Map.iter_keys !paths ~f:(fun node -> print_s [%message (node,
                Map.find_exn !paths node : string * string list)]); print_s
                [%message "\n" next_node]; *)
             (paths
                := match
                     Map.add
                       !paths
                       ~key:next_node
                       ~data:(Lambda_soup_utilities.get_title current_node)
                   with
                   | `Ok result -> result
                   | _ -> !paths);
             Queue.enqueue
               to_visit
               (File_fetcher.fetch_exn how_to_fetch ~resource:next_node));
      bfs (depth - 1)
  in
  bfs max_depth;
  Map.iter_keys !paths ~f:(fun node ->
    print_s [%message (node, Map.find_exn !paths node : string * string)]);
  let rec create_path node =
    if String.equal node (Lambda_soup_utilities.get_title start_page)
    then []
    else node :: create_path (Map.find_exn !paths node)
  in
  let result = create_path destination in
  print_s [%message (result : Page.t list)];
  None
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
