(* FIXME: Running this tool will generate a bisectX.out in current directory. *)

open Bisect

let source_and_coverage
    : string -> (int * int) list -> string list  * string list
  = fun src points ->

  let chan = open_in src in

  let rec process points (src, cov) = match input_line chan with
    | line_src ->
       let end_off = pos_in chan in
       let (current, remaining) =
	 List.partition (fun (o, _) -> o < end_off) points in
       let (min_visits, visited, unvisited) =
         List.fold_left
           (fun (m, v, u) nb -> (min m nb, v || nb > 0, u || nb = 0))
           (max_int, false, false)
           (List.map snd current) in
       let line_cov = if unvisited then "0"
		      else if visited then string_of_int min_visits
		      else "null" in
       process remaining (line_src :: src, line_cov :: cov)
    | exception End_of_file -> close_in chan ;
			       (List.rev src, List.rev cov)
    in process points ([], [])

(* FIXME:
 * Assumes that all files contains data about
 * the same set of files. *)
let coverage_data
    : string list -> (string * int array) list
  = fun files ->
  if files = [] then []
  else
    let combine a1 a2 = Array.mapi (fun i v -> v + a2.(i)) a1 in
    let data = List.map Common.read_runtime_data files in
    let hd = List.hd data in
    let tl = List.tl data in
    List.map (fun (k, v) ->
	      (k, try List.map (List.assoc k) tl
		      |> List.fold_left combine v
		  with Not_found -> [||])) hd

let _ =

  let prefix = ref "" in
  let cov_files = ref [] in

  let usage = "usage: coveralls [options] coverage*.out" in

  let options = Arg.align [
		    "--prefix", Arg.Set_string prefix,
		    " Prefix to add in order to find source and cmp files." ;
		  ] in

  Arg.parse options (fun s -> cov_files := s :: !cov_files) usage ;

  let coverage_data = coverage_data !cov_files in

  List.map (fun (src, cov) ->
	    let len = Array.length cov in
	    let src' = !prefix ^ "/" ^ src in
	    let pts = Common.read_points src'
		      |> List.map ( fun p -> ( p.Common.offset,
					       if p.Common.identifier < len
					       then cov.(p.Common.identifier)
					       else 0 ) ) in
	    (src, source_and_coverage src' pts)) coverage_data
  |> List.iter (fun (src, (_, cov)) ->
		let null = List.filter ((=) "null") cov |> List.length in
		let zero = List.filter ((=) "0") cov |> List.length in
		Printf.printf "%s: %d KO / %d null / %d OK\n"
			      src zero null (List.length cov - null - zero))
