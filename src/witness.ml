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
       
let pp_witness_datatype fd i d =
  output_string fd ((if i = 0 then "datatype " else "and ") ^ d.d_name ^ " =\n");

  List.iteri (fun i d  -> match d with
                       | PSR_Defncom _ -> ()
                       | PSR_Rule dr -> output_string fd ((if i = 0 then "    " else "  | ") ^
                                                            (String.capitalize_ascii dr.drule_name) );
                                        List.iter (fun (_, st) -> pp_premise fd st ) dr.drule_premises;
                                        output_string fd "\n" ) d.d_rules;
  output_string fd "\n"
       
let pp_witness_datatypes fd rd =
  List.iter (fun rd -> match rd with
                         FDC _ -> ()
                        | RDC dc -> List.iteri  (pp_witness_datatype fd) dc.dc_defns) rd
  
let generate (rd : relationsdefn )  (filename : string ) : unit =
  Printf.eprintf "generate witness\n";
  let fd = open_out (filename ^ ".thy") in
  output_string fd "theory Witness\nimports Main\nbegin\n";
  pp_witness_datatypes fd rd;
  output_string fd "end\n";
  close_out fd
