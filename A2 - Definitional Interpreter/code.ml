type exp = Const of int
			| Plus of exp*exp
			| Sub of exp*exp
			| Mul of exp*exp
			| Div of exp*exp
			| Mod of exp*exp
			| Abs of exp
			| Var of string
			| Bol of bool
			| Not of exp
			| And of exp*exp
			| Or of exp*exp
			| Imp of exp*exp
			| Eq of exp*exp
			| Grt of exp*exp
			| Less of exp*exp
			| Gre of exp*exp
			| Lee of exp*exp
			| Proji of int*exp array
			| Projb of int*exp array;;
				
let create_tuple n = Array.make n (Const(0));;

let set_tuple t n e = Array.set t n e;;

type opcode =     CONST of int 
				| BOL of bool 
				| VAR of string 
				| PLUS 
				| SUB 
				| MUL 
				| DIV 
				| MOD 
				| ABS 
				| NOT 
				| AND 
				| OR 
				| IMP 
				| EQ 
				| GRT 
				| LESS 
				| GRE 
				| LEE
				| PROJI 
				| PROJB;;

type ans = { a : int ; b : bool };;

let create n b = { a=n; b=b};;

let rho s = match s with
      "x" -> create 4 true
      | _ -> create 0 false;;

let rec eval rho t = match t with
					Const n -> create n true
					|Bol b -> create 0 b
					| Var s -> (rho s)
					| Plus (t1,t2) -> create ((eval rho t1).a+(eval rho t2).a) true
					| Sub (t1,t2) -> create ((eval rho t1).a-(eval rho t2).a) true
					| Mul (t1,t2) -> create ((eval rho t1).a*(eval rho t2).a) true
					| Div (t1,t2) -> create ((eval rho t1).a/(eval rho t2).a) true
					| Mod(t1,t2) -> create ((eval rho t1).a mod (eval rho t2).a) true
					| Abs t1 -> create (abs (eval rho t1).a) true
					| Not t1 -> create 0 (not (eval rho t1).b)
					| And(t1,t2) -> create 0 ((eval rho t1).b && (eval rho t2).b)
					| Or(t1,t2) -> create 0 ((eval rho t1).b || (eval rho t2).b)
					| Imp(t1,t2) -> create 0 (not ((eval rho t1).b) || (eval rho t2).b)
					| Eq(t1,t2) ->  create 0 (((eval rho t1).a=(eval rho t2).a))
					| Grt(t1,t2) -> create 0 (((eval rho t1).a>(eval rho t2).a))
					| Less(t1,t2) -> create 0 (((eval rho t1).a<(eval rho t2).a))
					| Gre(t1,t2) -> create 0 (((eval rho t1).a>=(eval rho t2).a))
					| Lee(t1,t2) -> create 0 (((eval rho t1).a<=(eval rho t2).a))
					| Proji(n,t1) -> create (eval rho (Array.get t1 n)).a true
					| Projb(n,t1) -> create 0 (eval rho (Array.get t1 n)).b;;
							
let rec compile e = match e with
					Const n -> [CONST (n)]
					| Bol b -> [BOL (b)]
					| Var y -> [VAR (y)]
					| Plus(e1,e2) -> (compile e1)@(compile e2)@[PLUS]
					| Sub(e1,e2) -> (compile e1)@(compile e2)@[SUB]
					| Mul(e1,e2) -> (compile e1)@(compile e2)@[MUL]
					| Div(e1,e2) -> (compile e1)@(compile e2)@[DIV]
					| Mod(e1,e2) -> (compile e1)@(compile e2)@[MOD]
					| Abs e1 -> (compile e1)@[ABS]
					| Not e1 -> (compile e1)@[NOT]
					| And(e1,e2) -> (compile e1)@(compile e2)@[AND]
					| Or(e1,e2) -> (compile e1)@(compile e2)@[OR]
					| Imp(e1,e2) -> (compile e1)@(compile e2)@[IMP]
					| Eq(e1,e2) -> (compile e1)@(compile e2)@[EQ]
					| Grt(e1,e2) -> (compile e1)@(compile e2)@[GRT]
					| Less(e1,e2) -> (compile e1)@(compile e2)@[LESS]
					| Gre(e1,e2) -> (compile e1)@(compile e2)@[GRE]
					| Lee(e1,e2) -> (compile e1)@(compile e2)@[LEE]
					| Proji(n,e1) -> (compile (Array.get e1 n))@[PROJI]
					| Projb(n,e1) -> (compile (Array.get e1 n))@[PROJB];;

let table s = match s.[(String.length s)-1] with
      'i' -> create 1 true
     |'0' -> create 1 true
     |'1' -> create 1 true
     |'2' -> create 1 true
     |'3' -> create 1 true
     |'4' -> create 1 true
     |'5' -> create 1 true
     |'6' -> create 1 true
     |'7' -> create 1 true
     |'8' -> create 1 true
     |'9' -> create 1 true
     |'b' -> create 0 true
     | _ -> create 0 false;; 

let rec execute table stk opc = match (stk,opc) with
					  (s,[]) -> List.hd s
					| (s,VAR(y)::c) -> execute table ((table y)::s) c
					| (s,CONST(n)::c) -> execute table ((create n true)::s) c
					| (s,BOL(b)::c) -> execute table ((create 0 b)::s) c
					| (n2::n1::s,PLUS::c) -> execute table ((create (n1.a+n2.a) true)::s) c
					| (n2::n1::s,SUB::c) -> execute table ((create (n1.a-n2.a) true)::s) c
					| (n2::n1::s,MUL::c) -> execute table ((create (n1.a*n2.a) true)::s) c
					| (n2::n1::s,DIV::c) -> execute table ((create (n1.a/n2.a) true)::s) c
					| (n2::n1::s,MOD::c) -> execute table ((create (n1.a mod n2.a) true)::s) c
					| (n1::s,ABS::c) -> execute table ((create (abs n1.a) true)::s) c
					| (n1::s,NOT::c) -> execute table ((create 0 (not n1.b))::s) c
					| (n2::n1::s,AND::c) -> execute table ((create 0 (n1.b && n2.b))::s) c
					| (n2::n1::s,OR::c) -> execute table ((create 0 (n1.b || n2.b))::s) c
					| (n2::n1::s,IMP::c) -> execute table ((create 0 ((not n1.b) || n2.b))::s) c
					| (n2::n1::s,EQ::c) -> execute table ((create 0 (n1.a=n2.a))::s) c
					| (n2::n1::s,GRT::c) -> execute table ((create 0 (n1.a>n2.a))::s) c
					| (n2::n1::s,LESS::c) -> execute table ((create 0 (n1.a<n2.a))::s) c
					| (n2::n1::s,GRE::c) -> execute table ((create 0 (n1.a>=n2.a))::s) c
					| (n2::n1::s,LEE::c) -> execute table ((create 0 (n1.a<=n2.a))::s) c
					| (n::s,PROJI::c) -> execute table (n::s) c
					| (n::s,PROJB::c) -> execute table (n::s) c;;


