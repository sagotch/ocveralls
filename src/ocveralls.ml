(* FIXME: Running this tool will generate a bisectX.out file
 * in current directory. *)
(* TODO: allow the use of "repo_token" field instead of
 * "service_job_id" and "service_name" pair. *)
(* NOTE: depend on a modified version of [ezjsonm] lib. Waiting for
   PR to be merged or not. *)

open OcverallsBisect
open OcverallsCI

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

  let (service_name, service_job_id) = ci_infos () in

  [ ("service_job_id", J.string service_job_id) ;
    ("service_name", J.string service_name) ;
    ("source_files",
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
	   ] |> J.dict))]
  |> J.dict
  |> J.to_channel ~minify:true stdout
