module Util = Edn_mermaid_util

(* TODO: support option *)

type node_id = string [@@deriving cconv]
type shape = S | R | C | A | RB [@@deriving cconv]
type node_opts = { text: string;
                   shape: shape [@default S]; } [@@deriving cconv]
type node = node_id * node_opts [@@deriving cconv]

type link = A | O | D | T [@@deriving cconv]
type connection_opts = { text: string [@default ""];
                         link: link [@default A]; } [@@deriving cconv]
type connection = node_id * node_id * connection_opts [@@deriving cconv]
type flow = connection list [@@deriving cconv]
type subgraph = string * flow [@@deriving cconv]
type graph = { direction: string;
               nodes: node list;
               subgraphs: subgraph list [@default []];
               flow: flow; } [@@deriving cconv]


let write_node buf (id, {text; shape}) =
  Buffer.add_string buf id;

  if Util.is_blank text
  then ()
  else begin match shape with
    | S -> Buffer.add_char buf '['
    | R -> Buffer.add_char buf '('
    | C -> Buffer.add_string buf "(("
    | A -> Buffer.add_char buf '>'
    | RB -> Buffer.add_char buf '{'
  end;

  Buffer.add_char buf '\"';
  Buffer.add_string buf text;
  Buffer.add_char buf '\"';

  begin match shape with
    | S -> Buffer.add_char buf ']'
    | R -> Buffer.add_char buf ')'
    | C -> Buffer.add_string buf "))"
    | A -> Buffer.add_char buf ']'
    | RB -> Buffer.add_char buf '}'
  end

let write_link buf { text; link } =
  match (link, Util.is_blank text) with
  | (A, true) -> Buffer.add_string buf " --> "
  | (A, false) ->
    Buffer.add_string buf "-- \"";
    Buffer.add_string buf text;
    Buffer.add_string buf "\" -->"
  | (O, true) -> Buffer.add_string buf " --- "
  | (O, false) ->
    Buffer.add_string buf "-- \"";
    Buffer.add_string buf text;
    Buffer.add_string buf "\" --- "
  | (D, true) -> Buffer.add_string buf " -.-> "
  | (D, false) ->
    Buffer.add_string buf "-. \"";
    Buffer.add_string buf text;
    Buffer.add_string buf "\" .-"
  | (T, true) -> Buffer.add_string buf " ==> "
  | (T, false) ->
    Buffer.add_string buf "== \"";
    Buffer.add_string buf text;
    Buffer.add_string buf "\" ==> "

let write_flow buf nodes flow =
  let get_node_by_id node_id =
    List.find (fun (id, _) -> id = node_id) nodes
  in
  let write_connection (na_id, nb_id, copts) =
    (* 1. write na *)
    write_node buf (get_node_by_id na_id);

    (* 2. write connection *)
    write_link buf copts;

    (* 3. write nb *)
    write_node buf (get_node_by_id nb_id);

    Buffer.add_char buf '\n';
  in
  List.iter write_connection flow

let write_subgraphs buf g =
  let write_subgraph (text, flow) =
    Buffer.add_string buf "subgraph ";
    Buffer.add_string buf text;
    Buffer.add_char buf '\n';

    write_flow buf g.nodes flow;

    Buffer.add_string buf "end\n";
  in
  List.iter write_subgraph g.subgraphs

let write buf g =
  Buffer.add_string buf "graph ";
  Buffer.add_string buf (String.uppercase_ascii g.direction);
  Buffer.add_char buf ';';
  Buffer.add_char buf '\n';
  write_subgraphs buf g;
  write_flow buf g.nodes g.flow

let to_edn contents =
  Edn_cconv.of_string_exn decode_graph contents

let to_mermaid contents =
  let buf = Buffer.create 256 in
  write buf (to_edn contents);
  Buffer.to_bytes buf
