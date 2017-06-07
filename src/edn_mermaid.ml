open Printf

module Util = Edn_mermaid_util
module Flowchart = Edn_mermaid_flowchart
module Sequence = Edn_mermaid_sequence

let () =
  let path = Sys.argv.(1) in
  let write_path = Filename.chop_extension path ^ ".mmd" in
  printf "A new mermaid file will be created at: %s.\n" write_path;

  begin match Util.determine_graph_type path with
  | ("sequence", contents) ->
    Util.write_file write_path (Sequence.to_mermaid contents);
  | ("flowchart", contents) ->
    Util.write_file write_path (Flowchart.to_mermaid contents);
  | _ ->
    exit(1)
  end;

  print_endline "Create successfully!.";
  printf "A new mermaid file will be created at: %s.\n" (write_path ^ ".png");
  exit( Sys.command ("mermaid -e $(which phantomjs) -p "
                     ^ write_path
                     ^ " -o " ^ Filename.dirname path) )
