open Crowbar
open Test02
open Test02_cgen

let check_test02 _ = true
       
let () =
  add_test ~name:"test02" [ e_gen ] @@ fun m -> check ( check_test02 m)
