(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (structs, functions) =
  let report_error e = raise (Failure e) in
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Cheez" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  and struct_type n  = L.named_struct_type context n in
  (* non-primitive types *)
  let array_t = fun (llvm_type) -> L.struct_type context [| L.pointer_type llvm_type; i32_t; L.pointer_type llvm_type |] in
  let string_t = L.pointer_type (L.i8_type context) in

  (* Types of struct field in llvm *)
  let rec struct_field_types = function
      A.Struct n -> struct_type n
    | A.Int -> i32_t
    | A.Float -> float_t
    | A.String -> string_t
    | A.Bool -> i1_t
    | _ -> raise (Failure("The type of struct field can't be defined"))
  in

  (* Struct declaration *)
  let structs_decls =
    let struct_decl map sdecl =

      (* Get struct name*)
      let check_field_types = (fun (t,_) -> struct_field_types t) in

      (* Get field types *)
      let name = sdecl.ssname in
      let rec get_field_types f = function
        | [] -> []
        | h :: t -> f h :: get_field_types f t
      in
      let field_types_list = (get_field_types check_field_types sdecl.sfields) in

      (* Convert list to array b/c L.struct_type only take in arrays *)
      let field_types_arr = Array.of_list field_types_list in

      (* Struct type *)
      let stype = L.struct_type context field_types_arr in

    (* Add struct in the form of (struct type, struct fields) to the symbol table *)
    StringMap.add name (stype, sdecl.sfields) map in

  (* Add each struct to the symbol table *)
  List.fold_left struct_decl StringMap.empty structs
  in

  let check_struct s =
    try StringMap.find s structs_decls
    with Not_found -> raise (Failure ("struct not found")) in

  (* Primitive types in llvm *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
    | A.Array a -> array_t (ltype_of_typ a)
    (* struct type is a list of types of fields *)
    | A.Struct n -> fst (check_struct n)
  in

  (* Type of array elements in llvm *)
  let rec _ltype_of_array_element = function
      A.Array a -> ltype_of_typ a
    | _ -> raise (Failure("Array element type is not supported"))
  in

  (* Print functions *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  (*Self implemented string functions*)
  let stringupper_t : L.lltype =
    L.function_type string_t [| string_t |] in
  let stringupper_func : L.llvalue =
    L.declare_function "upper" stringupper_t the_module in

  let stringlower_t : L.lltype =
    L.function_type string_t [| string_t |] in
  let stringlower_func : L.llvalue =
    L.declare_function "lower" stringlower_t the_module in

  let stringsubstring_t : L.lltype =
      L.function_type string_t [| string_t ; i32_t ; i32_t |] in
  let stringsubstring_func : L.llvalue =
      L.declare_function "substring" stringsubstring_t the_module in
  (*string built-in functions from the C string library*)
  let strcmp_t : L.lltype =
    L.function_type i1_t [| string_t; string_t |] in
  let strcmp_func : L.llvalue =
    L.declare_function "strcmp" strcmp_t the_module in

  let str_concat_t : L.lltype =
    L.function_type string_t [| string_t ; string_t|] in
  let str_concat_f : L.llvalue =
    L.declare_function "str_concat" str_concat_t the_module in
  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls =
    let function_decl map fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_ ) -> ltype_of_typ t ) fdecl.sformals) in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) map in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let _char_format_str = L.build_global_stringptr "%s\n" "" builder
    and int_format_str = L.build_global_stringptr "%d\n" "" builder
    and float_format_str = L.build_global_stringptr "%f\n" "" builder
    and bool_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  (* local variables *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          StringMap.add n (local, A.Void) m
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
   (* loop up for variables *)
    let lookup map n = match StringMap.find_opt n map with
        Some (v, _) -> v
      | None -> try fst (StringMap.find n local_vars)
                with Not_found -> report_error("Could not find " ^ n)
    in
    (* Construct code for an expression; return its value *)
    let rec expr map builder ((_, expression) : sexpr) = match expression with
        SIntLit i     ->  L.const_int i32_t i, map, builder
      | SFloatLit f   ->  L.const_float float_t f, map, builder
      | SBoolLit b    ->  L.const_int i1_t (if b then 1 else 0), map, builder
      | SStringLit s  ->  L.build_global_stringptr s "" builder, map, builder
      | SArrayLit a ->
        (* get array element type *)
        let array_type = ltype_of_typ (fst (List.hd a))in
        (* get array size in bytes *)
        let array_size = L.const_int i32_t (List.length a) in
        (* store array data *)
        let array_alloca = L.build_array_alloca array_type array_size "array_data_alloca" builder in
        let element_store (idx, builder) ev =
            let element_val, map1, builder = expr map builder ev in
            let idx_i = L.const_int i32_t idx in
            let element_ptr = L.build_gep array_alloca [| idx_i |] "element_pointer" builder in
            ignore(L.build_store element_val element_ptr builder);
            (idx + 1, builder) in
        let end_idx, builder = List.fold_left element_store (0, builder) a in
        (* allocate memory for array *)
        let llvm_array_type = array_t (array_type) in
        let alloca = L.build_alloca llvm_array_type "alloca" builder in
        (* array data field *)
        let array_data = L.build_struct_gep alloca 0 "array_data" builder in
        (* array length field *)
        let array_length = L.build_struct_gep alloca 1 "array_length" builder in
        (* array endpointer field *)
        let array_endptr = L.build_struct_gep alloca 2 "array_endpointer" builder in
        (* store *)
        ignore(L.build_store array_size array_length builder);
        ignore(L.build_store array_alloca array_data builder);
        let end_idx_i = L.const_int i32_t (end_idx - 1) in
        let end_ptr = L.build_gep array_alloca [| end_idx_i |] "end_pointer" builder in
        ignore(L.build_store end_ptr array_endptr builder);
        let array_ptr = L.build_load alloca "array_ptr" builder in
        (array_ptr, map, builder)
      | SArrayIndex(v, i) -> let ival, map1, builder = expr map builder i in
                              (match snd v with
                                SId s ->
                                  let array_data = L.build_struct_gep (lookup map s) 0 "array_data" builder in
                                  let data_ptr = L.build_load array_data "data_pointer" builder in
                                  let element_ptr = L.build_gep data_ptr [| ival |] "element_pointer" builder in
                                  let element_val = L.build_load element_ptr "element_value" builder in
                                  (element_val, map, builder)
                              | _ -> raise (Failure ("No such array exists")))
      | SId s -> L.build_load (lookup map s) s builder, map, builder
      | SAssignOp (v, e) -> let (e1, map1, builder) = expr map builder e in
                            (match (snd v) with
                              SId s -> ignore(L.build_store e1 (lookup map s) builder); (e1, map1, builder)
                            | _ -> raise (Failure("No such variable exists")))
      | SArrayAssignOp(v, i, e) -> let i1, map1, builder = expr map builder i in
                                   let e1, map2, builder = expr map builder e in
                                      (match snd v with
                                        SId s ->
                                          let array_data = L.build_struct_gep (lookup map s) 0 "array_data" builder in
                                          let data_ptr = L.build_load array_data "data_pointer" builder in
                                          let element_ptr = L.build_gep data_ptr [| i1 |] "element_pointer" builder in
                                          ignore(L.build_store e1 element_ptr builder);
                                          (e1, map2, builder)
                                      | _ -> raise (Failure ("No such array exists")))
      | SStructAssign (v, f, e) ->
        (* Evaluate assignment expression *)
        let rval, map1, builder = expr map builder e in
        let var = match snd v with
            SId s -> s
            | _ -> raise (Failure( "No such struct exists"))
        in

        (* Extract struct address from the lookup table *)
        let var_addr = lookup map1 var in

        (* Extract specified struct name and field name *)
        let struct_name, fname = (match snd (StringMap.find var map1) with
                             | A.Struct i -> (match f with
                                             A.Id i' -> (i, i')
                                             | _ -> raise (Failure("Invalid struct access")))
                             | _ -> raise (Failure("No such struct exists"))) in

        (* Extract field from the symbol table and modify the field *)
        let fields = snd (check_struct struct_name) in

            (* Convert tuple to a list for indexing *)
            let field_list = List.map (fun (_,nm) -> nm) fields in

            (* Helper function that locates the field and return its index within the field list *)
            let find_field_index lst ele =
              let index = ref(-1) in
              let () = Array.iteri (fun n elt -> if ele = elt then index := n else ()) (Array.of_list(lst))
              in !index

            in

            (* Get the index of specified field *)
            let index = (find_field_index field_list fname) in

        (* Construct pointer in memory *)
        let ptr = L.build_struct_gep var_addr index ("struct_p") builder in

        (* Write value of the evaluated expression in memory *)
        let _ = L.build_store rval ptr builder in

        (rval, map1, builder)

      | SStructAccess (v, f) ->
        (* Extract the wanted struct name *)
        let name = match snd v with
            SId s -> s
           | _ -> raise (Failure("Struct name is not found"))
        in

        (* Extract struct address from the lookup table *)
        let var_addr = lookup map name in

        (* Extract specified struct name and field name *)
        let struct_name, fname = (match snd (StringMap.find name map) with
            A.Struct i -> (match f with
                            | A.Id i' -> (i, i')
                            | _ -> raise(Failure("Invalid Field Name")))
           | _ -> raise (Failure("Struct name is not found"))) in

        (* Extract all fields of the struct *)
        let fields = snd (check_struct struct_name) in

            (* Convert tuple to a list for indexing *)
            let field_list = List.map (fun (_,nm) -> nm) fields in

            (* Helper function that locates the field and return its index within the field list *)
            let find_field_index lst ele =
              let index = ref(-1) in
              let () = Array.iteri (fun n elt -> if ele = elt then index := n else ()) (Array.of_list(lst))
              in !index

            in
            (* Get index of specified field *)
            let index = (find_field_index field_list fname) in

        (* Construct pointer of the wanted field *)
        let ptr = L.build_struct_gep var_addr index ("struct_p") builder in

        (* Load value of the wanted field *)
        let value = L.build_load ptr "field_v" builder in
        (value, map, builder)

      | SNot (e) -> let (e', _, _) = expr map builder e in
                    (L.build_not e' "not operation" builder, map, builder)
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
    	  let (e1', _, _) = expr map builder e1
    	  and (e2', _, _) = expr map builder e2 in
    	  (match op with
    	    A.Add     -> L.build_fadd
    	  | A.Sub     -> L.build_fsub
    	  | A.Mul     -> L.build_fmul
    	  | A.Div     -> L.build_fdiv
    	  | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
    	  | A.Neq     -> L.build_fcmp L.Fcmp.One
    	  | A.Lt    -> L.build_fcmp L.Fcmp.Olt
    	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
    	  | A.Gt -> L.build_fcmp L.Fcmp.Ogt
    	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
    	  | A.And | A.Or ->
    	      raise (Failure("Semant should have rejected and/or on float"))
        | _ -> raise (Failure("Operation is not defined"))
    	  ) e1' e2' "float op" builder, map, builder

      | SBinop (e1, op, e2) ->
    	  let (e1', _, _) = expr map builder e1
    	  and (e2', _, _) = expr map builder e2 in
    	  (match op with
    	    A.Add     -> L.build_add
    	  | A.Sub     -> L.build_sub
    	  | A.Mul     -> L.build_mul
        | A.Div     -> L.build_sdiv
    	  | A.And     -> L.build_and
    	  | A.Or      -> L.build_or
    	  | A.Eq   -> L.build_icmp L.Icmp.Eq
    	  | A.Neq     -> L.build_icmp L.Icmp.Ne
    	  | A.Lt    -> L.build_icmp L.Icmp.Slt
    	  | A.Leq     -> L.build_icmp L.Icmp.Sle
    	  | A.Gt -> L.build_icmp L.Icmp.Sgt
    	  | A.Geq     -> L.build_icmp L.Icmp.Sge
        | _ -> raise (Failure("Operation is not defined"))
    	  ) e1' e2' "general op" builder, map, builder
      | SCall ("prints", [e]) ->
        let e', _, builder = expr map builder e in
        L.build_call printf_func [| string_format_str ; e' |] "printf" builder, map, builder
      | SCall ("printi", [e]) ->
        let e', _, builder = expr map builder e in
        L.build_call printf_func [| int_format_str ; e' |] "printf" builder, map, builder
      | SCall ("printf", [e]) ->
        let e', _, builder = expr map builder e in L.build_call printf_func [| float_format_str ; e' |] "printf" builder, map, builder
      (* Print bools*)
      | SCall ("printb", [e]) ->
        let e', _, builder = expr map builder e in L.build_call printf_func [| bool_format_str ; e' |] "printb" builder, map, builder
      | SCall ("str_lower", [e]) ->
        let e', _, builder = expr map builder e in
        L.build_call stringlower_func [| e' |] "lower" builder, map, builder

      | SCall ("str_upper", [e]) ->
        let e', _, builder = expr map builder e in
        L.build_call stringupper_func [| e' |] "upper" builder, map, builder
      |SCall ("substring", [e]) ->
        let  e', _, builder = expr map builder e in
        L.build_call stringsubstring_func [| e' |] "substring" builder, map, builder

      | SCall ("strcmp", [e1; e2]) ->
        let  e1', _, builder = expr map builder e1 in
        let  e2', _, builder = expr map builder e2 in
        L.build_call strcmp_func  [| e1'; e2' |] "strcmp" builder, map, builder

      | SCall ("str_concat", [e1 ; e2]) ->
        let  e1', _, builder = expr map builder e1 in
        let  e2', _, builder = expr map builder e2 in
        L.build_call str_concat_f [| e1' ; e2' |] "str_concat" builder, map, builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
    	  let llargs = List.map (fun(a,b,c) -> a) (List.rev (List.map (expr map builder) (List.rev args))) in
    	  let result = (match fdecl.styp with
                          A.Void -> ""
                        | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder, map, builder
      | _ -> raise (Failure("Expression is not defined"))
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr = match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt map builder s = match s with
        SBlock sl -> let b, _ = List.fold_left (fun (b, m) s -> stmt m b s) (builder, map) sl in (b, map)
      | SExpr e -> ignore(expr map builder e); builder, map
      | SReturn e -> ignore(match fdecl.styp with
                              A.Void -> L.build_ret_void builder
                            | _ -> let e',_,_ = (expr map builder e) in L.build_ret e' builder ); builder, map
      | SVarDecl(ty, v, rex) ->
          (* e.g. ty = Array of Int; v = a; rex = ArrayLit of IntLit 1,2,3*)
          (match ty with
            (* declare struct *)
            A.Struct s ->
              let l_type = ltype_of_typ ty in
              let addr = L.build_alloca l_type v builder in
              let m' = StringMap.add v (addr, ty) map in
              (builder, m')
          | _ ->
              let l_type = ltype_of_typ ty in
              let addr = L.build_alloca l_type v builder in
              let rval, m', builder = expr map builder rex in
              let m'' = StringMap.add v (addr, A.Void) m' in
              let _ = L.build_store rval addr builder in
          (builder, m''))
      | SVarInitial(t, v) ->
        (match t with
          A.Struct s->
            let l_type = ltype_of_typ t in
            let addr = L.build_alloca l_type v builder in
            let m' = StringMap.add v (addr, t) map in
            (builder, m')
        |  _ ->
            let l_type = ltype_of_typ t in
                let addr = L.build_alloca l_type v builder in
                let m'' = StringMap.add v (addr, A.Void) map in
            (builder, m''))
      | SWhile(condition, stmtList) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore(L.build_br pred_bb builder);
          let body_bb = L.append_block context "while_body" the_function in
          let body_bldr, m' = stmt map (L.builder_at_end context body_bb) stmtList in
          add_terminal body_bldr (L.build_br pred_bb);
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val, _, _ = expr m' pred_builder condition in
          let merge_bb = L.append_block context "merge" the_function in
          ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb, m'
      | SFor(e1, e2, e3, stmtList) -> stmt map builder ( SBlock [ e1 ; SWhile (e2, SBlock [stmtList ; SExpr e3]) ] )
      | SIf(e, s1, s2) ->
        let bool_val, m', builder = expr map builder e in
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in
        let then_bb = L.append_block context "then" the_function in
        let then_builder, m'' = stmt m' (L.builder_at_end context then_bb) s1 in
          add_terminal then_builder build_br_merge;
        let else_bb = L.append_block context "else" the_function in
        let else_builder, m'' = stmt m' (L.builder_at_end context else_bb) s2 in
          add_terminal else_builder build_br_merge;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb, m'
      | _ -> report_error "No implementation"
      in

    (* Build the code for each statement in the function *)
    let builder,_= stmt StringMap.empty builder (SBlock fdecl.sfstmts) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
                            A.Void -> L.build_ret_void
                          | A.Float -> L.build_ret (L.const_float float_t 0.0)
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in
  List.iter build_function_body functions;
  the_module
