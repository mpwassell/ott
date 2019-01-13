open Types
open Grammar_pp

let down_st stnb = match stnb.st_es with
    [ Ste_st ( _ , St_node (_,stnb)) ] -> stnb

let down_list stnb = match stnb.st_es with
    [ Ste_list ( _,  stli :: _  ) ] -> match stli with
                                         Stli_listform stlb -> match stlb.stl_elements with
                                                                 [ Ste_st ( _ , st) ] -> match st with
                                                                        | St_node (_,stnb) when stnb.st_prod_name = "formula_judgement" ->
                                                                           (down_st ( down_st stnb )).st_prod_name
                                                                 
let pp_premise fd st =

  match st with
  | St_node (_,stnb) when stnb.st_prod_name = "formula_judgement" ->
     let dt = (down_st ( down_st stnb )).st_prod_name in 
     output_string fd (" " ^ dt)
  | St_node (_,stnb) when stnb.st_prod_name = "formula_dots" ->
     let dt = down_list stnb in
     output_string fd  (" " ^ "\"" ^ dt ^ " list \"")
  | _ ->  ()

let mk_premise st =
  match st with
  | St_node (_,stnb) when stnb.st_prod_name = "formula_judgement" ->
     let dt = (down_st ( down_st stnb )).st_prod_name in Some (Lang_nonterm (dt,(dt,[])))
(*  | St_node (_,stnb) when stnb.st_prod_name = "formula_dots" ->
     let dt = down_list stnb in 
     output_string fd  (" " ^ "\"" ^ dt ^ " list \"")*)
  | _ ->  None

            
let mk_prod (d : defn)  ( r : processed_semiraw_rule ) = match r with
  | PSR_Defncom _ -> None
  | PSR_Rule dr -> Some (
    {
      prod_name = dr.drule_name;
      prod_flavour = Bar;
      prod_meta = false;
      prod_sugar = false;
      prod_categories = StringSet.empty;
      prod_es = Auxl.option_map (fun (_,st) -> mk_premise st)  dr.drule_premises;
      prod_homs = [];
      prod_disambiguate = None;
      prod_bs = [];
      prod_loc = dummy_loc
    })
      
      
            
let mk_witness_rule ( d : defn ) = {
    rule_ntr_name = d.d_name;
    rule_ntr_names = [ (d.d_name,[]) ];
    rule_pn_wrapper = String.capitalize_ascii d.d_name;
    rule_ps = Auxl.option_map (mk_prod d) d.d_rules;
    rule_homs = [];
    rule_meta = false;
    rule_semi_meta = false;
    rule_phantom = false;
    rule_judgement = false;
    rule_loc = dummy_loc }
    
            
let pp_witness_datatype fd xd i d =

  output_string fd ((if i = 0 then "datatype " else "and ") ^ d.d_name ^ " =\n");

  List.iteri (fun i d  -> match d with
                       | PSR_Defncom _ -> ()
                       | PSR_Rule dr -> output_string fd ((if i = 0 then "    " else "  | ") ^
                                                            (String.capitalize_ascii dr.drule_name) );
                                        List.iter (fun (_, st) -> pp_premise fd st ) dr.drule_premises;
                                        output_string fd "\n" ) d.d_rules;
  output_string fd "\n"
       
let pp_witness_datatypes fd xd rd =
  List.iter (fun rd -> match rd with
                         FDC _ -> ()
                        | RDC dc -> List.iteri  (pp_witness_datatype fd xd) dc.dc_defns) rd

            
let mk_xx rd = List.concat  (List.map (fun rd -> match rd with
                                            FDC _ -> []
                                          | RDC dc -> List.map mk_witness_rule dc.dc_defns) rd)
                                             
let generate (rd : relationsdefn )  (filename : string ) ( xd : syntaxdefn ) sd : unit =
  Printf.eprintf "generate witness\n";
  (*let fd = open_out (filename ^ ".thy") in
  output_string fd "theory Witness\nimports Main\nbegin\n";
  pp_witness_datatypes fd xd rd;
  output_string fd "end\n";*)

  let rs = mk_xx rd in
  let xd = {
      xd_mds = [];
      xd_rs = rs;
      xd_dep = List.map (fun x -> (x,empty_dependencies)) (["ascii"; "ocaml";"isa"]);
      xd_srs = [];
      xd_srd = { srd_proper_subntr_data=[]; srd_subrule_graph=[]; srd_subrule_pn_promotion=[] };
      xd_crs = [];
      xd_axs = [];
      xd_sbs = [];
      xd_fvs = [];
      xd_embed_preamble = [];
      xd_embed = [];
      xd_isa_imports = [];
      xd_pas = { pa_data = [] }
    } in

  let deps = List.map 
      (fun x -> (x, Dependency.compute_dependencies xd x)) 
      (["ascii";"ocaml"; "isa"]) in
  let xd = { xd with xd_dep = deps } in
  
  let sd = {
      syntax = xd;
      relations = [];
      structure = [ ("../../ott/z3.ott", Struct_rs (List.map (fun r -> r.rule_ntr_name ) xd.xd_rs)) ] ;
      sources = sd.sources
    } in
  let pp_isa_opts_default = Isa {
      ppi_isa_primrec = false;
      ppi_isa_inductive = true;
      ppi_fancy_syntax = false;
      ppi_generate_lemmas = false;
      isa_library = ref ("",[])
    } in

  let lookup = Term_parser.make_parser xd in 
  Printf.eprintf "%s\n" (pp_syntaxdefn pp_ascii_opts_default xd );
  System_pp.pp_systemdefn_core_io pp_isa_opts_default sd lookup
    [("out.thy", ["../../ott/z3.ott"])] false
  (*close_out fd*)

let pp_ascii_struct_entry stre =
  match stre with
    Struct_md mvr -> Printf.eprintf " mvr=%s\n" mvr
  | Struct_rs ntrs -> List.iter (fun i -> Printf.eprintf " %s " i) ntrs
  | _ -> ()
