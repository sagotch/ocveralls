open Bisect

let source_and_coverage src points  =

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
		      else "0" in
       process remaining (line_src :: src, line_cov :: cov)
    | exception End_of_file -> close_in chan ;
			       (List.rev src, List.rev cov)
    in process points ([], [])

(* usage: [coveralls foo.ml coverageXXX.out path] *)

let _ =

  let src_file = Sys.argv.(1) in
  let cov_file = Sys.argv.(2) in
  let dir = Sys.argv.(3) in

  let cov = Common.read_runtime_data cov_file
	    |> List.assoc src_file in

  let points = Common.read_points (dir ^ src_file)
	       |> List.map ( fun p -> ( p.Common.offset,
					if p.identifier < Array.length cov
					then cov.(p.identifier)
					else 0 ) ) in

  let (src, cov) = source_and_coverage (dir ^ src_file) points
  in
  List.iter2 (fun cov line -> Printf.printf "[%s]%s\n" cov line)
	     cov src
