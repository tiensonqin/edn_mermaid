module Util = Edn_mermaid_util

type participant = string [@@deriving cconv]

(**
   S     ->      Solid line without arrow
   D     -->     Dotted line without arrow
   SA    ->>     Solid line with arrowhead
   DA    -->>    Dotted line with arrowhead
   SC    -x      Solid line with a cross at the end (async)
   DC    --x     Dotted line with a cross at the end (async)
*)
type arrow = S | D | SA | DA | SC | DC [@@deriving cconv]

type message = participant * participant * string * arrow [@@deriving cconv]

type note = participant * string * string [@@deriving cconv]

type loop = string * message list [@@deriving cconv]

type alt = string * message list * message list [@@deriving cconv]

type opt = string * message list [@@deriving cconv]

type sequence = { participants: participant list [@default []];
                  notes: note list [@default []];
                  loops: loop list [@default []];
                  alts: alt list [@default []];
                  opts: opt list [@default []];
                  messages: message list; } [@@deriving cconv]

let write_participants buf participants =
  let write_participant p =
    Buffer.add_string buf "participant ";
    Buffer.add_string buf p;
    Buffer.add_char buf '\n';
  in
  List.iter write_participant participants

let write_arrow buf arrow =
  let s = match arrow with
    | S -> "->"
    | D -> "-->"
    | SA -> "->>"
    | DA -> "-->>"
    | SC -> "-x"
    | DC -> "--x"
  in
  Buffer.add_string buf s

let write_messages buf messages =
  let write_message (pa, pb, body, arrow) =
    Buffer.add_string buf pa;
    write_arrow buf arrow;
    Buffer.add_string buf pb;
    Buffer.add_string buf ": ";
    Buffer.add_string buf body;
    Buffer.add_char buf '\n';
  in
  List.iter write_message messages

let write_notes buf notes =
  let write_note (participant, position, body) =
    Buffer.add_string buf "Note ";
    begin match position with
      | "left" -> Buffer.add_string buf "left of "
      | "right" -> Buffer.add_string buf "right of "
      | _ -> Buffer.add_string buf "over "
    end;
    Buffer.add_string buf participant;
    Buffer.add_string buf ": ";
    Buffer.add_string buf body;
    Buffer.add_char buf '\n';
  in
  List.iter write_note notes

let write_loops buf loops =
  let write_loop (text, messages) =
    Buffer.add_string buf "loop ";
    Buffer.add_string buf text;
    Buffer.add_char buf '\n';
    write_messages buf messages;
    Buffer.add_string buf "end";
    Buffer.add_char buf '\n';
  in
  List.iter write_loop loops

let write_alts buf alts =
  let write_alt (text, then_messages, else_messages) =
    Buffer.add_string buf "alt ";
    Buffer.add_string buf text;
    Buffer.add_char buf '\n';
    write_messages buf then_messages;
    Buffer.add_string buf "else";
    Buffer.add_char buf '\n';
    write_messages buf else_messages;
    Buffer.add_string buf "end";
    Buffer.add_char buf '\n';
  in
  List.iter write_alt alts

let write_opts buf opts =
  let write_opt (text, messages) =
    Buffer.add_string buf "opt ";
    Buffer.add_string buf text;
    Buffer.add_char buf '\n';
    write_messages buf messages;
    Buffer.add_string buf "end";
    Buffer.add_char buf '\n';
  in
  List.iter write_opt opts

let write buf g =
  Buffer.add_string buf "sequenceDiagram";
  Buffer.add_char buf '\n';
  write_participants buf g.participants;
  write_notes buf g.notes;
  write_loops buf g.loops;
  write_alts buf g.alts;
  write_opts buf g.opts;
  write_messages buf g.messages

let to_edn contents =
  Edn_cconv.of_string_exn decode_sequence contents

let to_mermaid contents =
  let buf = Buffer.create 256 in
  write buf (to_edn contents);
  Buffer.to_bytes buf
