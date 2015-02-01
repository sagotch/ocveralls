(* FIXME: Running this tool will generate a bisectX.out file
 * in current directory. *)
(* NOTE: depend on a modified version of [ezjsonm] lib. Waiting for
   PR to be merged or not. *)

open OcverallsBisect

module J = Ezjsonm

let _ =

  let prefix = ref "." in
  let cov_files = ref [] in

  let usage = "usage: coveralls [options] coverage*.out" in

  let options =
    Arg.align [
	"--prefix", Arg.Set_string prefix,
	" Prefix to add in order to find source and cmp files." ;
      ] in

  Arg.parse options (fun s -> cov_files := s :: !cov_files) usage ;

  let coverage_data = coverage_data !cov_files in

  List.map (fun (src, cov) ->
	    let len = Array.length cov in
	    let src' = !prefix ^ "/" ^ src in
	    let pts =
	      B.read_points src'
	      |> List.map ( fun p -> ( p.B.offset,
				       if p.B.identifier < len
				       then cov.(p.B.identifier)
				       else 0 ) ) in
	    (src, source_and_coverage src' pts)) coverage_data
  |> J.list
       (fun (fn, (src, cov)) ->
	[ ("name", J.string fn) ;
	  ("source", J.string (String.concat "\n" src)) ;
	  "coverage", J.list
			(function "null" -> J.unit ()
				| i -> J.int (int_of_string i)) cov
	]
	|> J.dict)
  |> J.to_channel ~minify:true stdout
