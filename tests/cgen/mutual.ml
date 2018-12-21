(* An attempt to define mutually recursive generators so that we can handle
   mutually recursive non-terminals in the grammar *)
open Crowbar


type var = string 
       
type pat = P_id of var | P_pair of pat*pat

type pexpr = PE of pat*expr
and expr = E_var of var | E_pexp of pexpr | E_lets of  (var * expr) list * expr 

let pair_gen (g1 : 'a gen) (g2 : 'b gen) : ('a*'b) gen = map [ g1 ; g2 ] (fun x y -> (x,y))        

let var_gen : string gen = const "x"

let pat_gen : pat gen = fix (fun pat_gen ->  choose [
                                             map [ var_gen ]  (fun x -> P_id x);
                                             map [ pat_gen; pat_gen ] (fun p1 p2 -> P_pair (p1,p2))])

let rec pexpr_gen = lazy (
                        map [ pat_gen; unlazy expr_gen ] (fun p e -> PE (p,e)))

and expr_gen = lazy ( choose [
                          map [ var_gen ] (fun x -> E_var x);
                          map [ unlazy pexpr_gen ] (fun pe -> E_pexp pe);
                          map [ list (pair_gen var_gen (unlazy expr_gen)) ; unlazy expr_gen ] (fun ps e -> E_lets (ps,e))
                 ])

let lazy expr_gen = expr_gen                 

                      

let rec  pp_e e =  match e with
    E_var x -> x
  | E_pexp pe -> (pp_pexpr pe)
  | E_lets (ps,e) -> "lets " ^ (String.concat " and " (List.map (fun (x,e) -> x ^ " = " ^ (pp_e e)) ps))
                                                       ^ " in " ^ (pp_e e)
and pp_pexpr pe = match pe with
    PE (p,e) -> "( " ^ (pp_pat p) ^ " = " ^ (pp_e e) ^ ") "
and pp_pat p = match p with
    P_id x -> x
  | P_pair (p1,p2) -> "( " ^  (pp_pat p1) ^ ", " ^ (pp_pat p2) ^ ")"
                                         

                            
let check_test e = Printf.eprintf "E=%s\n" (pp_e e);
  true
    
let () =
  add_test ~name:"test" [ expr_gen ] @@ fun m -> check ( check_test m)

