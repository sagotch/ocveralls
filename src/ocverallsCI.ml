(* Out-of-the-box supported CI list.
 * Fisrt element of a pair is the environement variable to test
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
