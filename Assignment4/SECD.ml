let stack = [];;
let dump = [];;
type var = string;;
exception EmptyStack;;
exception Not_Found;;

type exp =    Const of int 
			| Var of var
			| Plus of exp*exp
			| Sub of exp*exp
			| Mul of exp*exp
			| Div of exp*exp
			| Bol of bool
			| Eq of exp*exp
			| Grt of exp*exp
			| Less of exp*exp
			| Gre of exp*exp
			| Lee of exp*exp
			| Lam of var*exp
			| Ift of exp*exp*exp
			| Let of exp*exp
			| And of exp*exp
			| Or of exp*exp
			| Not of exp
			| Imp of exp*exp
			| Abs of exp
			| Proj of exp list*exp;; 

type opcode =     CONST of int 
				| BOL of bool 
				| VAR of var 
				| PLUS 
				| SUB 
				| MUL 
				| DIV 
				| AND
				| OR
				| NOT
				| IMP
				| ABS
				| EQ
				| GRT 
				| LESS 
				| GRE 
				| LEE
				| CLOS of var*opcode list
				| APP
				| RET
				| COND of opcode list*opcode list;;

type ans = BolA of bool | ConstA of int | Clos of table*var*opcode list and table = ans list;;

let rec compile e = match e with
				  Var(y) -> [VAR (y)]
				| Const n -> [CONST (n)]
				| Bol b -> [BOL (b)]
				| Lam(t,e1) -> [CLOS(t,(compile e1)@[RET])]
				| And(e1,e2) -> (compile e1)@(compile e2)@[AND]
				| Or(e1,e2) -> (compile e1)@(compile e2)@[OR]
				| Imp(e1,e2) -> (compile e1)@(compile e2)@[IMP]
				| Not (e) -> (compile e)@[NOT]
				| Abs (e) -> (compile e)@[ABS]
				| Plus(e1,e2) -> (compile e1)@(compile e2)@[PLUS]
				| Sub(e1,e2) -> (compile e1)@(compile e2)@[SUB]
				| Mul(e1,e2) -> (compile e1)@(compile e2)@[MUL]
				| Div(e1,e2) -> (compile e1)@(compile e2)@[DIV]
				| Eq(e1,e2) -> (compile e1)@(compile e2)@[EQ]
				| Grt(e1,e2) -> (compile e1)@(compile e2)@[GRT]
				| Less(e1,e2) -> (compile e1)@(compile e2)@[LESS]
				| Gre(e1,e2) -> (compile e1)@(compile e2)@[GRE]
				| Lee(e1,e2) -> (compile e1)@(compile e2)@[LEE]
				| Ift(e1,e2,e3) -> (compile e1)@[COND((compile e2),(compile e3))]
				| Let(e1,e2) -> (compile e1)@(compile e2)@[APP]
				| Proj(e1,e2) -> (match (e1,e2) with
									| ([],_) -> raise Not_Found
									| (x::xs,Const(0)) -> (compile x)
									| (x::xs,Const(n)) -> (compile (Proj(xs,Const(n-1))))
									| _ -> raise Not_Found 
								)

let rec execute stack table oplist dump = match (stack,table,oplist,dump) with
				  (s,t,[],d) -> ( match s with
				  					[] -> raise EmptyStack
				  					|_ -> List.hd s
				  				)
				| (s,t,CONST(n)::c,d) -> execute ([ConstA(n)]@s) t c d
				| (s,t,BOL(b)::c,d) -> execute ([BolA(b)]@s) t c d
				| (s,t,VAR(y)::lis,d) -> execute ([(List.hd t)]@s) t lis d	
				| ((BolA b1)::(BolA b2)::s,t,AND::c,d) -> execute ([BolA(b1&&b2)]@s) t c d 
				| ((BolA b1)::(BolA b2)::s,t,OR::c,d) -> execute ([BolA(b1||b2)]@s) t c d 
				| ((BolA b1)::(BolA b2)::s,t,IMP::c,d) -> execute ([BolA((not b1) || b2)]@s) t c d 
				| ((BolA b)::s,t,NOT::c,d) -> execute ([BolA (not b)]@s) t c d	
				| (ConstA(n)::s,t,ABS::c,d) -> execute ([ConstA (abs n)]@s) t c d					
				| (ConstA(n1)::ConstA(n2)::s,t,PLUS::c,d) -> execute ([ConstA(n1+n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,SUB::c,d) -> execute ([ConstA(n2-n1)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,MUL::c,d) -> execute ([ConstA(n1*n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,DIV::c,d) -> execute ([ConstA(n1/n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,EQ::c,d) -> execute ([BolA(n1==n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,GRT::c,d) -> execute ([BolA(n1>n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,LESS::c,d) -> execute ([BolA(n1<n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,GRE::c,d) -> execute ([BolA(n1>=n2)]@s) t c d
				| (ConstA(n1)::ConstA(n2)::s,t,LEE::c,d) -> execute ([BolA(n1<=n2)]@s) t c d
				| (BolA(true)::s,t,COND(c2,c3)::c,d) -> execute s t (c2@c) d
				| (BolA(false)::s,t,COND(c2,c3)::c,d) -> execute s t (c3@c) d
				| (ConstA(n)::Clos(t1,y,c1)::s,t,APP::c,d) -> execute [] ([(ConstA(n))]@t1) c1 ([(s,t,c)]@d)
				| (s,t,CLOS(v,c1)::c,d) -> execute ([Clos(t,v,c1)]@s) t c d
				| (s,t,RET::c,(s1,t1,c1)::d) -> execute ([List.hd s]@s1) t1 c1 d;;
				