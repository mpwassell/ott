open Types


let pp_cgen_preamble fd sd =
  output_string fd "(* Generated from Ott *)\n";
  List.iter (fun (_,hname,espec) ->
      Printf.eprintf "hname = %s\n" hname;
      if hname = "cgen" then
        List.iter (fun (Embed_string (_,s)) -> output_string fd s) espec
      else
        ()) sd.xd_embed
                

let pp_prod_gen fd p =
  let ntmvs = List.concat (List.map (fun ele -> match ele with
                                        Lang_nonterm (x,_) -> [x]
                                      | Lang_metavar (x,_) -> [x]
                                      | _ -> []) p.prod_es) in
  output_string fd "   map [ ";
  List.iter (fun nt -> output_string fd  (nt ^ "_gen; ")) ntmvs;
  output_string fd "(fun ";
  List.iter (fun nt -> output_string fd  (nt ^ " ")) ntmvs;
  output_string fd  (" -> " ^ p.prod_name ^ " ( ");
  List.iter (fun nt -> output_string fd  (nt ^ ",")) ntmvs;
  output_string fd ");\n"
                                                    
  
                 
                           
            
let pp_rule_gen fd r =
  let fname = (r.rule_ntr_name) ^ "_gen" in
  output_string fd ( "let " ^ fname ^  " : " ^ (r.rule_ntr_name) ^ " gen = fix (fun " ^ fname ^ " -> choose [ \n");
  List.iter (pp_prod_gen fd) r.rule_ps;
  output_string fd "])\n"
                
                
let pp_cgen_generators fd sd = List.iter (pp_rule_gen fd) sd.xd_rs 
                
let generate_cgen (sd : syntaxdefn )  (filename : string ) : unit = 
  let fd = open_out filename in
  pp_cgen_preamble fd sd;
  pp_cgen_generators fd sd;
  close_out fd


            
                     
