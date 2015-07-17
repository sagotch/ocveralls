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

(* Out-of-the-box supported CI list.
 * First element of a pair is the environement variable to test
 * Second element is the pair composed of the "service_name"
 * and of the environement variable containing "service_job_id" *)
let otb_support =
  ["TRAVIS", ("travis-ci", "TRAVIS_JOB_ID");
   "CIRCLECI", ("circleci", "CIRCLE_BUILD_NUM");
   "SEMAPHORE", ("semaphore", "REVISION");
   "JENKINS_URL", ("jenkins", "BUILD_ID");
   "CI_NAME", ("codeship", "CI_BUILD_NUMBER")]


(* Return the ("service_name", "service_job_id") pair
 * corresponding to the service ocveralls is running on. *)
let ci_infos () =
  let test_env s = try ignore (Sys.getenv s); true
                   with Not_found -> false in
  let (_, (service_name, service_job_id)) =
    List.find (fun (x, _) -> test_env x) otb_support in
  (service_name, Sys.getenv service_job_id)
