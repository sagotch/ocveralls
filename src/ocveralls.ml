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

module B = OcverallsBisect
module C = OcverallsCI
module J = OcverallsJSON

let _ =

  let prefix = ref "." in
  let repo_token = ref "" in
  let cov_files = ref [] in
  let output = ref "-" in

  let usage = "usage: coveralls [options] coverage*.out" in

  let options =
    Arg.align [
	"--output", Arg.Set_string output,
	" File where to dump json. Set to - for stdout." ;
	"--prefix", Arg.Set_string prefix,
	" Prefix to add in order to find source and cmp files." ;
	"--repo_token", Arg.Set_string repo_token,
	" Use repo token instead of automatic CI detection." ;
      ] in

  Arg.parse options (fun s -> cov_files := s :: !cov_files) usage ;

  let prefix = !prefix in
  let repo_token = !repo_token in
  let cov_files = !cov_files in
  let output = !output in

  let source_files =
    B.coverage_data cov_files
    |> List.map (fun (src, cov) ->
		 (src, B.coverage cov (prefix ^ "/" ^ src) ) )
    |> J.list
	 (fun (src, cov) ->
	  [ ("name", J.string src) ;
	    ("source_digest",
	     J.string (Digest.file (prefix ^ "/" ^ src) |> Digest.to_hex)) ;
	    ("coverage",
	     J.list (fun x -> if x = -1 then J.unit else J.int x) cov )
	  ] |> J.dict)

  in

  (if repo_token <> ""
   then [ ("repo_token", J.string repo_token) ;
	  ("source_files", source_files) ]
   else let (service_name, service_job_id) = C.ci_infos () in
	[ ("service_job_id", J.string service_job_id) ;
          ("service_name", J.string service_name) ;
	  ("source_files", source_files) ])
  |> J.dict
  |> J.to_channel (if output = "-" then stdout else open_out output)
