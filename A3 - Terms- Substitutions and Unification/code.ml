type symbol = { sym : string ; arity : int };;

let def_symbol s n = {sym = s ; arity = n};;

type variable = {name : string ; varity : int };;

let def_variable s n = {name= s ; varity=n};;

type term = V of variable | Node of symbol*(term list);;
(* assumption -> signature will be provided as input in form of symbol list using def_symbol. *)

let first (a,b) = a;;

let sec (a,b) = b;;

(* basically removes the first element of the list *)
let cut list = match list with
		x::xs -> xs;;

(* returns true if element a is not repeated *)
let rec finder list a i = match list with              
  			  [] -> if i>0 then false else true
  			| x::xs -> if x.sym=a then (finder xs a (1+i)) else (finder xs a i);; 

(* gives a list containing boolean denoting if it is repeated afterwards its first occurence *)
let rec convertor list = match list with
				  [] -> []
				| x:: xs -> (finder xs (x.sym) 0)::(convertor xs);;

(* returns false even if a single element is repeated *)
let rec parsing list = match list with
						  [] -> true
						| true::xs -> parsing xs
						| false::xs -> false;;

(* this is map only the only difference is that it takes two functions as input *)
let rec map2 f s l = match l with
				  [] -> []
				| x::xs -> (f s x)::(map2 f s xs);;

(* normal append function ; used to convert a  b' list list to b'list in vars *)
let rec append list = match list with
  			  [] -> []
  			| x::xs -> x@(append xs);;

let rec map f l = match l with
				  [] -> []
				| x::xs -> (f x)::(map f xs);;

(* returns max of a list *)
let rec maxlist list = match list with
			  	[] -> 0
				| x::xs -> if x>(maxlist xs) then x else (maxlist xs);;

(* returns sum of all the elements of a list *)
let rec sum list m = match list with
					  [] -> m
					| x::xs -> sum xs (m+x);;

(* this is my composition *)
let comp_helper f g = f@g;;

exception Invalid_Signature;;

(* checking that arity of every symbol is >=0 *)
let rec check_sig_arity list = match list with
  			  [] -> true
  			| hd::tl -> (if hd.arity<0 then raise Invalid_Signature else check_sig_arity tl);;

(* checking no symbol's name is repeated *)
let rec check_sig_sym list = parsing (convertor list);;

let check_sig list = if ((check_sig_arity list)&&(check_sig_sym list)) then true else raise Invalid_Signature;;

(* for checking well formed term *)
let rec finder_spl a list = match list with
  			[] -> false
  			| x::xs -> if ((x.sym=a.sym)&&(x.arity=a.arity)) then true else (finder_spl a xs);;

let rec wf list1 term= match term with
  		    V a -> if a.varity=0 then true else false;
   		  | Node (s,list2)-> (finder_spl s list1)&&(s.arity=(List.length list2))&&(parsing (map2 wf list1 list2));;

let rec size term = match term with
			  V a -> 0;
			| Node (s,list) -> 1+(sum (map size list) 0);;

let rec height term = match term with
			  V a -> 0
			| Node (s,list) -> if s.arity=0 then (maxlist (map height list)) else 1+(maxlist (map height list));;

let rec vars term  = match term with
				  V a -> [a]
				| Node (s,list) -> append (map vars list);;

(* substitution representation -> (term*term)list where first element is to be substituted by second element *)
(* in case of multi tuple list(substitution) the substitution appearing at lower index is to be applied first *)
let rec subst_helper sigma term = match term with
					  V a -> if (first (List.nth sigma 0))=(V a) then (sec (List.nth sigma 0)) else (V a)
					| Node(s,list) -> if s.arity=0 then (Node(s,list)) else (Node(s,(map2 subst_helper sigma list)));;	
	
let rec subst sigma term = match (sigma,term) with
					  (x::[],(V a)) -> if (first x)=(V a) then (sec x) else (V a)
					| (x::[],(Node(s,list))) -> if s.arity=0 then (Node(s,list)) else (Node(s,(map2 subst_helper [x] list)))
					| (x::xs,(V a)) -> if (first x)=(V a) then (sec x) else (V a)
					| (x::xs,(Node(s,list))) -> if s.arity=0 then (Node(s,list)) else (subst xs (Node(s,(map2 subst_helper [x] list))));;


exception NOT_UNIFIABLE;;

(* checks if a variable is not present in a term or is there *)
(* user must ensure the name of variable and the let xyz thing then xyz should be the name of the variable *)
let rec varfinder list v i = match list with              
  			  [] -> if i=0 then true else false
  			| x::xs -> if x.name=v.name then (varfinder xs v (1+i)) else (varfinder xs v i);; 

let helper_1 v t = if (varfinder (vars t) v 0) then [((V v),t)] else raise NOT_UNIFIABLE;;

(* applies sigma homomorphic extension to all elements of a term list *)
let rec list_subst sigma list = match list with
								[] -> []
								| hd::tl -> (subst_helper sigma hd)::(list_subst sigma tl);;
(* most general unifier *)
let rec mgu t1 t2  = match (t1,t2) with
					  (V v1,V v2) -> [(t1,t2)]
					| (V v1 , Node(s,list)) -> if s.arity=0 then [(t1,t2)] else (helper_1 v1 t2)
					| (Node(s1,[]),Node(s2,[])) -> if s1.sym=s2.sym then [(t1,t2)] else raise NOT_UNIFIABLE
					| (Node(s1,list1),Node(s2,list2)) -> if s1.sym=s2.sym then 
					(comp_helper (mgu (List.nth list1 0) (List.nth list2 0)) 
						(mgu (Node(s1,(list_subst (mgu (List.nth list1 0) (List.nth list2 0)) (cut list1)))) 
							(Node(s2,(list_subst (mgu (List.nth list1 0) (List.nth list2 0)) (cut list2)))))) 
				    else raise NOT_UNIFIABLE;;

