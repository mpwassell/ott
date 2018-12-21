(*
  Experimental code to pretty print Cowbar [1] generator code to freely generate expressions from the Ott grammar

  [1] https://github.com/stedolan/crowbar

 *)
open Types


let pp_cgen_preamble fd sd =
  output_string fd "(* Generated from Ott *)\n";
  List.iter (fun (_,hname,espec) ->
      Printf.eprintf "hname = %s\n" hname;
      if hname = "cgen" then
        List.iter (fun (Embed_string (_,s)) -> output_string fd s) espec
      else
        ()) sd.xd_embed

            
let rec pp_element_list es = List.concat (List.map pp_element  es)

and pp_element (ele : element ) :  (string*string) list = match ele with
  | Lang_nonterm (x,y) -> [ ( "(unlazy " ^  x ^ "_gen)" ,Grammar_pp.pp_plain_nonterm y)]
  | Lang_metavar (x,y) -> [ ( "(unlazy " ^  x ^ "_gen)" ,Grammar_pp.pp_plain_metavar y)]
  | Lang_list elb -> pp_list_form elb                                     
  | _ -> []
                                         
and pp_list_form (elb : element_list_body) : (string*string) list =

  let rec mk_pairs xs = match xs with
      x::xs -> (match pp_element x with
                  [] -> mk_pairs xs
                | s -> let s = String.concat " " (List.map fst s) in
                       match xs with
                        | [] -> s
                        | _ -> "pair " ^ s ^ " " ^ (mk_pairs xs) ^ "")
    | _ -> ""
  in
  [ ("list ( " ^ (mk_pairs elb.elb_es) ^ ")" , "xx") ]
            
let pp_prod_gen fd p =
  
  let ntmvs = pp_element_list p.prod_es in  
  output_string fd "   map [ ";

  (* If element is a list then want to emit "list gen" where gen is the appropriate gen
     but if we have a pair or n-tuple then want something more - just might be better to generate new gen for each list form
     So first walk over all rules/prods looking for list forms and subforms and build the list 
     of list gens - what if recursive with current gen being defined ??
  *)
  output_string fd (String.concat "; " (List.map fst ntmvs));
  output_string fd "] (fun ";
  output_string fd (String.concat " "  (List.map snd ntmvs));
  output_string fd  (" -> " ^ p.prod_name ^ " ( ");
  output_string fd (String.concat ", " (List.map snd ntmvs));
  output_string fd "));\n"
                                                    
  
let pp_rule_gen fd fun_prefix r =
  let fname = (r.rule_ntr_name) ^ "_gen" in
  output_string fd ( fun_prefix  ^ fname ^ " = lazy (choose [ \n");
  List.iter (fun p -> if p.prod_meta || p.prod_sugar then () else pp_prod_gen fd p) r.rule_ps;
  output_string fd "])\n"
                
                
let pp_cgen_generators fd sd = List.iter
             (fun (pr,r) -> if r.rule_judgement || r.rule_semi_meta || r.rule_meta || r.rule_phantom then
                         ()
                       else
                         pp_rule_gen fd pr r)
             (List.combine ("let rec " :: (List.init (List.length sd.xd_rs - 1) (fun _ -> " and "))) sd.xd_rs)

let pp_cgen_lazy_pat fd sd = List.iter
             (fun r -> if r.rule_judgement || r.rule_semi_meta || r.rule_meta || r.rule_phantom then
                         ()
                       else
                         output_string fd ("let lazy " ^ r.rule_ntr_name ^ "_gen = " ^ r.rule_ntr_name ^ "_gen\n"))
             sd.xd_rs

             
let generate_cgen (sd : syntaxdefn )  (filename : string ) : unit = 
  let fd = open_out filename in
  pp_cgen_preamble fd sd;
  pp_cgen_generators fd sd;
  pp_cgen_lazy_pat fd sd;
  close_out fd


            
                     
