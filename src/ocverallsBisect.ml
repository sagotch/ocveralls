(*
    Ocveralls - Generate JSON file for http://coveralls.io API
                from Bisect (http://bisect.x9c.fr) data.

    Copyright (C) 2015 Julien Sagot

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(* FIXME: Running this tool will generate a bisectX.out file
 * in current directory. because of the [at_exit] call in Bisect.Runtime. *)

module B = Bisect.Common

(** [source_and_coverage cov "foo.ml"] returns
    the list of lines read in "foo.ml" and the list of coverage
    metadata for each line ("0", "null", "1", ...) *)
let coverage
    : int array -> string -> int list
  = fun cov src ->
  let len = Array.length cov in
  let points =
    List.map ( fun p -> ( p.B.offset,
			  if p.B.identifier < len
			  then cov.(p.B.identifier)
			  else 0 ) )
	     (B.read_points src) in

  let chan = open_in src in

  (* For a line and some points:
   * - Split points in two groups:
   *   points contained in the line, and the rest.
   * - Determine if every contained points has been visited and
   *   attribute a coverage metadata ("0", "null", ...) to the line.
   * - Restart with next line and reamining points. *)
  let rec process points cov = match input_line chan with
    | line_src ->
       let end_off = pos_in chan in
       let (current, remaining) =
	 List.partition (fun (o, _) -> o < end_off) points in
       let (min_visits, visited, unvisited) =
         List.fold_left
           (fun (m, v, u) nb -> (min m nb, v || nb > 0, u || nb = 0))
           (max_int, false, false)
           (List.map snd current) in
       let line_cov = if unvisited then 0
		      else if visited then min_visits
		      else -1 in
       process remaining (line_cov :: cov)
    | exception End_of_file -> close_in chan ;
			       List.rev cov
		in process points []

(** From a file list, read and combine coverage data. *)
let coverage_data
    : string list -> (string * int array) list
  = fun files ->
  (* Combine all files runtime data:
   * point coverage is the sum of counters of
   * each file associated to it. *)
  let combine a1 a2 =
    (* If [a1] and [a2] have not the same length,
     * [mapi] on the longest one, and assume [0] for missing
     * values in the shortest. *)
    let len1 = Array.length a1 and len2 = Array.length a2 in
    let long, short, len = if len1 > len2 then a1, a2, len2
                           else a2, a1, len1  in
    Array.mapi (fun i v -> v + (if i < len then short.(i) else 0)) long in
  let data = List.map B.read_runtime_data files |> List.flatten in
  (* For each file in data,
   * Combine information in data abouth this file. *)
  List.map fst data
  |> List.sort_uniq compare
  |> List.map
       (fun file ->
	let data = List.find_all (fun (x, _) -> x = file) data
		   |> List.map snd in
	(file, List.fold_left combine (List.hd data) (List.tl data)))
