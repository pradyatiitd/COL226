type edit = {marker : int ref ; arr : char list ref ; tail : char ref};;

let rec str_list s = match s with
  "" -> []
  |s -> s.[0]::str_list (String.sub s 1 ((String.length s)-1));; 

let create s = match (String.length s) with
  0 -> {marker = ref 0; arr = ref [] ; tail = ref ' '}
  | _ -> {marker = ref 0; arr = ref (str_list s) ; tail = ref s.[(String.length s)-1]};;

exception AtLast;;
exception Empty;;
exception AtFirst;;
exception TooShort;;

let update t w = 
  if !(t.marker)=(lgh t)-1 then t.tail:=w else t.tail := last t;;

let lgh t = List.length !(t.arr);;

let nonempty t = 
	if (lgh t)=0 then false else true;;

let last t =  match (!(t.arr)) with
  [] -> raise Empty
  | _ -> !(t.tail);;

let concat t1 t2 = match (!(t2.arr)) with
    [] -> if (!(t1.arr))=[] then raise Empty else t1.arr := (!(t1.arr)); t1.tail := (last t1)
  | _ -> t1.arr := (!(t1.arr))@(!(t2.arr)); t1.tail := (last t2);;

let rec revhelper list = match list with
  			[] -> []
  			| x::xs -> (revhelper xs)@[x];;

let first t = match (!(t.arr)) with
  [] -> raise Empty
  | _ -> List.hd (!(t.arr));;

let reverse t = match (!(t.arr)) with
  [] -> raise Empty
  | _ ->  t.tail := (first t);t.arr :=  revhelper (!(t.arr));;

let forward t = 
  if !(t.marker) = (lgh t) - 1 then raise AtLast else t.marker := (!(t.marker)) + 1;;

let back t = 
  if !(t.marker)=0 then raise AtFirst else t.marker := (!(t.marker)) -1;;

let moveto t n = 
	if n >= (lgh t) then raise TooShort else t.marker := n;;   

let rec replace list nth w out i= match (i,list) with
     (_,[]) -> out
      |(c,x::xs) -> if c=nth then (replace xs nth w (out@[w]) (c+1)) else (replace xs nth w (out@[x]) (c+1));; 

let empty_list = [];;

let rep t w = match (!(t.arr)) with
   [] -> raise TooShort
  |_ -> t.arr := (replace (!(t.arr)) (!(t.marker)) w empty_list 0);;  


