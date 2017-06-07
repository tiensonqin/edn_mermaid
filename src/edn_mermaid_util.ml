open Printf

let read_file file =
  let ic = open_in file in
  let buf = Buffer.create (in_channel_length ic) in
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
    done; assert false
  with End_of_file ->
    Buffer.contents buf

let write_file file contents =
  let oc = open_out file in
  fprintf oc "%s" contents;
  close_out oc

let is_blank s =
  s = ""

let str_contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let determine_graph_type file =
  let contents = read_file file
  in
  let contains = fun pattern -> str_contains contents pattern
  in
  if contains "participants" && contains "messages"
  then
    ("sequence", contents)
  else if contains "direction" && contains "nodes" && contains "flow"
  then
    ("flowchart", contents)
  else
    ("", contents)
