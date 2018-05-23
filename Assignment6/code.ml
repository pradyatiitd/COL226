(* declaring exceptions *)
exception NOT_UNIFIABLE;;
exception GOAL_NOT_FULFILLED;;

(* defining types *)
type symbol = string*term list and term = Var of string|Const of int|Sym of symbol;;
type atom = Atom of symbol;;
type body = Body of atom list;;
type head = Head of atom;;
type clause = Fact of head | Rule of head*body;;
type program = clause list;;

(* obtaining elements from a tuple *)
let first a = match a with
			(x,y) -> x;;

let second a = match a with
			(x,y) -> y;;

let sec x = match x with
			(x,y,z) -> y;;

(* composition of mgu *)
let comp_helper f g = f@g;;

(* works like List.map but instead of one takes two arguments *)
let rec map f s l = match l with
				  [] -> []
				| x::xs -> (f s x)::(map f s xs);;

(* returns a list after chopping off its head *)
let cut_list a = match a with
				[] -> []
				| x::xs -> xs;;

(* returns true if and only if there is no occurence of v in list *)
let rec varfinder list v i = match list with              
  			  [] -> if i=0 then true else false
  			| x::xs -> if x=v then (varfinder xs v (1+i)) else (varfinder xs v i);; 

(* for appending the list of lists obtained after List.map *)
let rec append list = match list with
  			  [] -> []
  			| x::xs -> x@(append xs);;

(* returns the list of all the variables present in that term *)
let rec vars term  = match term with
				  Var a -> [a]
				| Const t -> []
				| Sym (s,list) -> append (List.map vars list);;

(* for substituting a variable with a symbol *)
let helper v t = if (varfinder (vars t) v 0) then [((Var v),t)] else raise NOT_UNIFIABLE;;

(*  substitution on a single term *)
let rec subst_helper sigma term = match term with
					  Var a -> if (first (List.nth sigma 0))=(Var a) then (second (List.nth sigma 0)) else (Var a)
					| Const c -> (Const c)
					| Sym(s,tList1) -> Sym(s,(map subst_helper sigma tList1));;	

(* substitution on whole list *)
let rec list_subst sigma list1 = match (sigma,list1) with
				  ([],list1) -> list1
				| (_,[]) -> []
				| (s::rest,hd::tl) -> (list_subst rest (map subst_helper [s] list1));;

(* returns the string of the variable *)
let var_str r = match r with
			Var x -> Char.code(x.[0]);;

(* finding the mgu *)
let mgu_V2 term1 term2 = 
	let rec mgu t1 t2 = match (t1,t2) with
						  (Var v1,Var v2) -> ( if (var_str t1)>=97 && (var_str t1)<=122 && (var_str t2)>=97 && (var_str t2)<=122 then
						  					 		(if(v1=v2) then [] else raise NOT_UNIFIABLE)
						  						(* else ([(t1,t2)])	 *)
						  					else ( if (var_str t1)>=65 && (var_str t1)<=90 && (var_str t2)>=65 && (var_str t2)<=90 then 
						  							(if v1=v2 then [] else [(t1,t2)])
						  						else [(t1,t2)]
						  							)
						  					 )
						| (Var v1,Const c) -> [(t1,t2)]
						| (Var v1,Sym (s,tList)) -> (helper v1 t2)
						| (Const c1,Const c2) -> if c1=c2 then [(t1,t2)] else raise NOT_UNIFIABLE
						| (Const c1,_) -> raise NOT_UNIFIABLE
						| (Sym(s1,[]),Sym(s2,[])) -> if s1=s2 then [] else raise NOT_UNIFIABLE
						| (Sym(s1,tList1),Sym(s2,tList2)) -> if s1=s2 && ((List.length tList1)=(List.length tList2)) && (not (List.length tList1=0)) then 
							(comp_helper (mgu (List.nth tList1 0) (List.nth tList2 0))
								(mgu (Sym(s1,(list_subst (mgu (List.nth tList1 0) (List.nth tList2 0)) (cut_list tList1)))) 
									 (Sym(s2,(list_subst (mgu (List.nth tList1 0) (List.nth tList2 0)) (cut_list tList2))))
								)
							) 
							else raise NOT_UNIFIABLE
						| (_,_) -> raise NOT_UNIFIABLE
	in try (mgu term1 term2) with NOT_UNIFIABLE -> [(Const(-1),Const(-2))];;

(* gives us symbol from a clause(fact/rule) *)
let clostorm t = match t with
				  Fact(Head(Atom(s,tList))) -> Sym(s,tList)
				| Rule(Head(Atom(s,tList)),Body(_)) -> Sym(s,tList);;

(* gets term from an atom *)
let atmtosym r = match r with
		Atom(s1,list1) -> Sym(s1,list1);;

(* gives a list of all the possible various goals and their corresonding output list *)
let rec stker_V2 proglist goallist out = match (proglist,goallist) with
		  ([],_) -> []
		| (x::xs,y::ys) ->( match x with
							| Fact(Head(Atom(s1,list1))) -> if not ((mgu_V2 y (clostorm x))=[(Const(-1),Const(-2))]) then 
															[((list_subst (mgu_V2 y (clostorm x)) (ys)),(out@(list_subst (mgu_V2 y (clostorm x)) [y])))]@(stker_V2 xs (y::ys) out) 
													  else (stker_V2 xs (y::ys) out)
							| Rule(Head(Atom(_,_)),Body(_)) -> []
						);;

(* defining facts *)
let f1 = Fact(Head(Atom("edge",[Var "a";Var "b"])));;
let f2 = Fact(Head(Atom("edge",[Var "a";Var "c"])));;
let f3 = Fact(Head(Atom("edge",[Var "a";Var "d"])));;
let f4 = Fact(Head(Atom("edge",[Var "b";Var "e"])));;
let f5 = Fact(Head(Atom("edge",[Var "b";Var "f"])));;
let f6 = Fact(Head(Atom("edge",[Var "c";Var "g"])));;
let f7 = Fact(Head(Atom("edge",[Var "c";Var "h"])));;
let f8 = Fact(Head(Atom("edge",[Var "d";Var "i"])));;
let f9 = Fact(Head(Atom("edge",[Var "d";Var "j"])));;
let r0 = Rule(Head(Atom("path",[Var "X1";Var "X1"])),Body([]));;
let r1 = Rule(Head(Atom("path",[Var "X";Var "Y"])),Body([Atom("edge",[Var "X";Var "Z"]);Atom("path",[Var "Z";Var "Y"])]));;

(* program list *)
let prog1 = [f1;f2;f3;f4;f5;f6;f7;f8;f9;r0;r1];;

(* true if idx is at last+1 *)
let isLength n = if n=(List.length prog1) then true else false;;

(* execution with backtracking *)
let rec ronaldo idx goalList stack output = match (idx,goalList,stack,output) with
  (_,[],stk,out) -> out
  							(* (	match stk with
  										| [] -> out
  										| (prog,pglist,o)::xs -> if pglist=[] then out else out@(append pglist)
  										) *)
| (n,y::ys,stack,out) -> if (isLength n) then
											( match stack with
												| (l1,l2)::s -> (ronaldo 0 l1 s l2) 
												| [] -> raise GOAL_NOT_FULFILLED
											)
										else ( match (List.nth prog1 n) with 
										| Fact(Head(Atom(s1,list1))) -> if (mgu_V2 y (clostorm (List.nth prog1 n)))=[(Const(-1),Const(-2))] then (ronaldo (n+1) (y::ys) stack out)
										    else (ronaldo 0 (list_subst (mgu_V2 y (clostorm (List.nth prog1 n))) ys) ((cut_list (stker_V2 prog1 (y::ys) out))@stack) (out@(list_subst (mgu_V2 y (clostorm (List.nth prog1 n))) [y])))
										| Rule(Head(Atom(s1,list1)),Body(list2)) -> if (mgu_V2 (clostorm (List.nth prog1 n)) y)=[(Const(-1),Const(-2))] then (ronaldo (n+1) (y::ys) stack out)
													else (ronaldo 0 (list_subst (mgu_V2 (clostorm (List.nth prog1 n)) y) ((List.map atmtosym list2)@ys)) stack out)
									);;







										