open Crowbar
open Test01
open Test01_cgen

let check_test01 _ = true
       
let () =
  add_test ~name:"test01" [ t_gen ] @@ fun m -> check ( check_test01 m)
