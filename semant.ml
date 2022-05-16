(* Semantic checking for the Cheez compiler. Modeled from MicroC. *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (structs, functions) =
  let _decl_struct map sd =
    let dup_err = "struct dup error"
    and make_err er = raise (Failure er)
    and n = sd.sname
    in match sd with
      _ when StringMap.mem sd.sname map -> make_err dup_err
      | _ -> StringMap.add n sd map
  in
  let check_struct struc =
    let _symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty struc.fields
    in
      {
      ssname = struc.sname;
      sfields = struc.fields;
      }
  in

	let add_func map fd =
		let _built_in_err = "function " ^ fd.fname ^ "may not be redefined"
		and dup_err = "duplicate function " ^ fd.fname
		and raise_err err = raise (Failure err)
		and n = fd.fname
		in match fd with
			| _ when StringMap.mem n map -> raise_err dup_err
			| _ ->  StringMap.add n fd map
	in
		let built_in_decls = List.fold_left add_func StringMap.empty [
      {typ = Void; fname = "prints"; formals = [(String, "args")];  fstmts = [] };
      {typ = Void; fname = "printi"; formals = [(Int, "args")];  fstmts = [] };
      {typ = Void; fname = "printf"; formals = [(Float, "args")];  fstmts = [] };
			{typ = Void; fname = "printb"; formals = [(Bool, "args")];  fstmts = [] };
			{typ = String; fname = "str_concat"; formals = [(String, "args")];  fstmts = [] };
			{typ = String; fname = "substring"; formals = [(String, "args")];  fstmts = [] };
			{typ = String; fname = "str_lower"; formals = [(String, "args")];  fstmts = [] };
			{typ = String; fname = "str_upper"; formals = [(String, "args")];  fstmts = [] };
			{typ = Bool; fname = "strcmp"; formals = [(String, "args")];  fstmts = [] };

      ]
 	 in


	let function_decls = List.fold_left add_func built_in_decls functions
	in
	let find_func s =
	try StringMap.find s function_decls
	with Not_found -> raise (Failure ("unrecognized function " ^ s))
	in
	let _ = find_func "main" in
	let check_function func =
		let add_var map (tp, name, len) =
			let dup_err = "Variable: " ^ name ^" is a duplicate." in
			match (tp, name) with
			_ when StringMap.mem name map -> raise (Failure dup_err)
			| _ -> StringMap.add name (tp, name, len) map
		in
		let check_assign lvaluet rvaluet err =
			if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in
		let type_of_identifier s symbols =
			let (ty, _, _) = try StringMap.find s symbols with Not_found -> raise( Failure("ID not found: " ^ s))
		in ty in
		let rec check_expr map e = match e with
			IntLit  l 	-> (Int, SIntLit l, map)
		| FloatLit l 	-> (Float, SFloatLit l, map)
		| BoolLit l  	-> (Bool, SBoolLit l, map)
		| StringLit l -> (String, SStringLit l, map)
    | StructAccess(v, f) ->
				(match v with
						Id i -> let lt, vname, map1 = find_name v map "Illegal assignment" in
										(Int, SStructAccess((lt, vname), f), map1)
						| _ -> raise(Failure("Invalid identifier: " ^ string_of_expr v)))
    | StructAssign(v, f, e)->
        (match v with
            Id i -> let lt, vname, map1 = find_name v map "Illegal assignment" in
										let rt, ex, map2 = check_expr map1 e in
										(check_assign Ast.Int Ast.Int "Type mismatch", SStructAssign((lt, vname), f, (rt, ex)), map2)
            | _ -> raise(Failure("Invalid identifier: " ^ string_of_expr v)))
		| ArrayLit l -> let array_ele = List.map (check_expr map) l in
										let array_type,_,_ = List.nth array_ele 0 in  (*type of array is the type of 1st element*)
										let rec type_consist lst =
											(match lst with
											| [] -> []
											| (typ1,styp1,map1) :: tl -> if typ1 != array_type then raise (Failure ("Type inconsistent " ^ string_of_typ array_type ^ " " ^ string_of_typ typ1))
																								  else (typ1,styp1,map1) :: (type_consist tl))
										in
										let get_sexpr (typ1,styp1,map1) = (typ1,styp1) in
										let checked_array_ele = type_consist array_ele in
										(Array array_type, SArrayLit(List.map get_sexpr checked_array_ele), map)
		| ArrayIndex(s, i) -> let sname = (match s with
																				Id s -> s
																			| _ -> raise (Failure ("Invalid identifier"))) in
													let st, sst, map1 = check_expr map s in
														(match st with
															Array(ele_type) -> let it,sit,map2 = check_expr map1 i in
																									(match sit with
																										SIntLit l -> let (_,_,lsize) = StringMap.find sname map in
																																(if l >= lsize then raise (Failure ("Index out of bound"))
																																else (ele_type, SArrayIndex((st, sst), (it, sit)), map2))
																									| SId s -> (if it != Int then raise (Failure ("Index must be integer"))
																														 else (ele_type, SArrayIndex((st, sst), (it, sit)), map2))
																									| _ -> raise (Failure ("Index must be integer")))
														| _ -> raise (Failure ("Variable not an array")))

		(*| ArrayReverseOp(e) -> let lt, vname, map1 = (match e with
															Id s -> find_name e map "reverse id not found error"
															|_ -> check_expr map e) in
														(match lt with
															Array(ele_type) -> (ele_type, SArrayReverseOp((lt,vname)), map1)

															| _ -> raise(Failure("not array type error"))) *)
		(*| ArraySizeOp(e) ->
			let lt, vname, map1 = (match e with
															Id s -> find_name e map "reverse id not found error"
															|_ -> check_expr map e) in
														(match lt with
															Array(ele_type) -> (ele_type, SArraySizeOp((lt,vname)), map1)
															| _ -> raise(Failure("not array type error")))*)
		| Id s       	-> (type_of_identifier s map, SId s, map)
		| AssignOp(v, e)	->
					let lt, vname, map1 = find_name v map "Illegal assignment" in
					let rt, ex, map2 = check_expr map1 e in
					(check_assign lt rt "Type mismatch", SAssignOp((lt, vname), (rt, ex)), map2)
		| ArrayAssignOp(v, i, e) ->
					let vname = (match v with
												Id s -> s
											| _ -> raise (Failure ("Invalid identifier"))) in
					let vt, svt, map1 = find_name v map "Illegal assignment" in
					let it,sit,map2 = check_expr map1 i in
					let et,set,map3 = check_expr map2 e in
					(match vt with
						Array(ele_type) -> 	(match sit with
																	SIntLit l -> let (_,_,lsize) = StringMap.find vname map in
																							(if l >= lsize then raise (Failure ("Index out of bound"))
																							else (check_assign ele_type et "type mismatch", SArrayAssignOp((ele_type, svt), (it, sit), (et, set)), map3))
																| SId s -> (if it != Int then raise (Failure ("Index must be integer"))
																					 else (check_assign ele_type et "type mismatch", SArrayAssignOp((ele_type, svt), (it, sit), (et, set)), map3))
																| _ -> raise (Failure ("Index must be integer")))
					| _ -> raise (Failure ("Variable not an array")))
		| Call(fname, args) as call ->
					let fd = find_func fname in
					let param_length = List.length fd.formals in
					if List.length args != param_length then
						raise (Failure ("expecting " ^ string_of_int param_length ^
										" arguments in " ^ string_of_expr call))
					else let check_call (ft, _) e =
					let (et, e', map') = check_expr map e in
					let err = "illegal argument found " ^ string_of_typ et ^
							" expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
					in (check_assign ft ft err, e')
					in
					let args' = List.map2 check_call fd.formals args
					in
					(fd.typ, SCall(fname, args'), map)

		| Not(e) as notEx-> let (t, e', map') = check_expr map e in
					if t != Bool then
						raise (Failure ("expecting bool expression in " ^ string_of_expr notEx))
					else (Bool, SNot((t, e')), map')
		| Binop(e1, op, e2) as ex ->
					let (t1, e1', map') = check_expr map e1 in
					let (t2, e2', map'') = check_expr map' e2 in
					if t1 = t2 then
					let ty =
						match t1 with
							_ -> match op with
								| Add | Sub | Mul | Div    when (t1 = Int || t1 = Float) -> t1
								| Add                      when t1 = String -> String
								| Eq | Neq                 -> Bool
								| Lt | Leq | Gt | Geq      when (t1 = Int || t1 = Float) -> Bool
								| And | Or                 when t1 = Bool -> Bool
								| _ -> raise (Failure ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr ex))
							in
					(ty, SBinop((t1, e1'), op, (t2, e2')), map'')
					else
						raise (Failure "binary oprands types do not match.")
		and find_name (name) map err =
			match name with
			Id _ -> check_expr map name
			| _ -> raise (Failure ("Cannot find name"))
		in
		let check_bool_expr map e =
			let (t, e', map') = check_expr map e in
			match t with
				Bool -> (t, e')
			| _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
		in

		let rec check_stmt_list map = function
			  [] -> ([], map)
			| Block sl :: sl' -> check_stmt_list map (sl @ sl')
			| s :: sl -> let (s1, map1) = check_stmt map s in
						 let (s2, map2) = check_stmt_list map1 sl in
						 (s1 :: s2, map2)
		and check_stmt map = function
			  Block sl -> (SBlock(fst (check_stmt_list map sl)), map)
			| Expr e -> let (typ, sexpr, new_map) = check_expr map e in
						(SExpr (typ, sexpr), new_map)
			| If(e, st1, st2) ->
				let sthen, _ = check_stmt map st1 in
				let selse, _ = check_stmt map st2 in
				(SIf(check_bool_expr map e, sthen, selse), map)
			| While(e, stList) -> SWhile(check_bool_expr map e, fst (check_stmt map stList)), map
			| For(e1, e2, e3, stList) -> let (st1, m') = check_stmt map e1 in
										   let (typ3, s3, m'') = check_expr m' e3 in
										   (SFor(st1, check_bool_expr m'' e2, (typ3, s3), fst (check_stmt m'' stList)), m'')
			| Return e -> let (t, e', map') = check_expr map e in
						  if t = func.typ then (SReturn (t, e'), map')
						  else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
										string_of_typ func.typ ^ " in " ^ string_of_expr e))
			| VarInitial(tp, id) -> let new_map = add_var map (tp, id, 0) in
									(SVarInitial(tp, id), new_map)
			| VarDecl(tp, id, e) ->
									let (right_ty, sexpr, map') = check_expr map e in
									let length = (match right_ty with
																  Array _ -> (match sexpr with
																 								SArrayLit l -> List.length l
																							| _ -> 0)
																| _ -> 0) in
									let new_map = add_var map' (tp, id, length) in
									let right = (right_ty, sexpr) in
									(SVarDecl(tp, id, right), new_map)
			| _ -> raise (Failure "Match failure")
		in

		let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty (func.formals)
		in
		{
			styp = func.typ;
			sfname = func.fname;
			sformals = func.formals;
			sfstmts = match fst (check_stmt symbols (Block(func.fstmts))) with
			SBlock(sl) -> sl
			| _ -> let err = "internal error: block didn't become a block?"
			in raise (Failure err)
		}

  in
  let sfunc = List.map check_function functions in
  let sstructs = List.map check_struct structs in
  (sstructs, sfunc)
