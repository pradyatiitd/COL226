exception Not_Found;;
exception Type_MisMatch;;

type exp = Const of int
		 | Bol of bool
		 | Var of string
		 | Plus of exp*exp
		 | Sub of exp*exp
		 | Mul of exp*exp
		 | Div of exp*exp
		 | Abs of exp
		 | Eq of exp*exp
		 | Grt of exp*exp
		 | Less of exp*exp
		 | Gre of exp*exp
		 | Lee of exp*exp
		 | And of exp*exp
		 | Or of exp*exp
		 | Not of exp
		 | Imp of exp*exp
		 | Lam of exp*exp 
		 | Let of exp*exp
		 | Ift of exp*exp*exp
		 | Proj of exp list * int;;

type env = (string*ans) list and ans = Const of int | Bol of bool | Ans of (env*exp);;

let rec lookup table y = match table with
				 [] -> raise Not_Found
				|(x1,x2)::xs -> if x1=y then x2 else (lookup xs y);;

let decode x = match x with
			Var y -> y;;

let rec eval e stk = match (e,stk) with
		 (Ans(t,Const n),[]) -> Const n
		|(Ans(t,Bol b),[]) -> Bol b
		|(Ans(t,Var x),stk) -> eval (lookup t (decode (Var x))) stk
		|(Ans(t1,Lam(e1,e2)),(Ans(t2,e3))::s) -> eval (Ans([((decode e1),Ans(t2,e3))]@t1,e2)) s
		|(Ans(t,Let(e1,e2)),s) -> eval (Ans((t,e1))) ([Ans(t,e2)]@s)
		|(Ans(t,Plus(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Const(n1+n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Sub(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Const(n1-n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Mul(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Const(n1*n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Div(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Const(n1/n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Eq(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Bol (n1=n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Grt(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Bol (n1>n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Less(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Bol (n1<n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Gre(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Bol (n1>=n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Lee(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Const n1,Const n2) -> eval (Ans(t,Bol (n1<=n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,And(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Bol n1,Bol n2) -> eval (Ans(t,Bol (n1&&n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Or(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Bol n1,Bol n2) -> eval (Ans(t,Bol (n1||n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Imp(e1,e2)),s)-> (match ((eval (Ans(t,e1)) s),(eval (Ans(t,e2)) s)) with
										| (Bol n1,Bol n2) -> eval (Ans(t,Bol ((not n1)||n2))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Not(e1)),s) -> (match (eval (Ans(t,e1)) s) with
										| (Bol b) -> eval (Ans(t,Bol (not b))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Abs(e1)),s) -> (match (eval (Ans(t,e1)) s) with
										| (Const n) -> eval (Ans(t,Const (abs n))) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Ift(e1,e2,e3)),s) -> (match (eval (Ans(t,e1)) s) with
										| (Bol true) -> eval (Ans(t,e2)) s
										| (Bol false) -> eval (Ans(t,e3)) s
										| _ -> raise Type_MisMatch
									)
		|(Ans(t,Proj(e,i)),s) -> ( match (e,i) with
										| ([],_) -> raise Type_MisMatch
										| (x::xs,0) -> eval (Ans(t,x)) s
										| (x::xs,i) -> eval (Ans(t,Proj(xs,i-1))) s
										| _ -> raise Type_MisMatch
								);;
		

		