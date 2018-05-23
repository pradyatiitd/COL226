%Basic types integer boolean
hastype(Gamma,const(X),intT).
hastype(Gamma,bool(X),boolT).

%necessary functions
lookup([],variable(X),T) :- fail.
lookup([p(variable(X),T)|Xs],variable(X),T) :- !.
lookup([p(variable(Y),Z)|Xs],variable(X),T) :- lookup(Xs,variable(X),T).

append([],L,L).
append([X|Xs],L2,[X|L3]) :- append(Xs,L2,L3).

%% look checks if a variable is found in a definition
look(variable(X),sim(variable(X),E)).
look(variable(X),seq(D1,D2)) :- look(variable(X),D1),look(variable(X),D2).
look(variable(X),par(D1,D2)) :- look(variable(X),D1),look(variable(X),D2).
look(variable(X),loc(D1,D2)) :- look(variable(X),D2).

%% uc returns if all variables are uncommon
uc(sim(variable(X),E),D) :- look(variable(X),D).
uc(D,sim(variable(X),E)) :- look(variable(X),D).
uc(D,seq(D1,D2)) :- uc(D,D1);uc(D,D2).
uc(seq(D1,D2),D) :- uc(D,D1);uc(D,D2).
uc(D,par(D1,D2)) :- uc(D,D1);uc(D,D2).
uc(par(D1,D2),D) :- uc(D,D1);uc(D,D2).
uc(D,loc(_,D2)) :- uc(D,D2).
uc(loc(_,D2),D) :- uc(D,D2).

%variable
hastype(Gamma,variable(X),Y) :- lookup(Gamma,variable(X),Y).

%mathematical operators
hastype(Gamma,plus(E1,E2),intT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT). 
hastype(Gamma,sub(E1,E2),intT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT). 
hastype(Gamma,mul(E1,E2),intT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT). 
hastype(Gamma,div(E1,E2),intT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,abs(E),intT) :- hastype(Gamma,E,intT).

%boolean operators
hastype(Gamma,andT(E1,E2),boolT) :- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,orT(E1,E2),boolT) :- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,impT(E1,E2),boolT) :- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,notT(E),boolT) :- hastype(Gamma,E,boolT).

%comparators
hastype(Gamma,less(E1,E2),boolT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,grt(E1,E2),boolT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,lesseq(E1,E2),boolT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,greq(E1,E2),boolT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,equal(E1,E2),boolT) :- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).

%% typeequals
hastype(Gamma,eqS(E1,E2),T) :- hastype(Gamma,E1,T),hastype(Gamma,E2,T).

%conditional
hastype(Gamma,ift(E1,E2,E3),X) :- hastype(Gamma,E1,boolT),hastype(Gamma,E2,X),hastype(Gamma,E3,X).

%functional expression
hastype(Gamma,abs(variable(X),E),arrow(T1,T2)) :- hastype([p(variable(X),T1)|Gamma],E,T2).

%function application
hastype(Gamma,pair(E1,E2),T2) :- hastype(Gamma,E1,arrow(T1,T2)),hastype(Gamma,E2,T2).

%mixed type
hastype(Gamma,lend(D,E),T) :- typeElaborates(Gamma,D,T1),append(T1,Gamma,T2),hastype(T2,E,T).

%tuples
hastype(Gamma,tuple([E]),cartesian([T])) :- hastype(Gamma,E,T).
hastype(Gamma,tuple([E|Es]),cartesian([T|Ts])) :- hastype(Gamma,E,T),hastype(Gamma,tuple(Es),cartesian(Ts)).

%projections
hastype(Gamma,proj(tuple([]),const(N)),T) :- fail.
hastype(Gamma,proj(tuple([X|Xs]),const(0)),T) :- hastype(Gamma,X,T).
hastype(Gamma,proj(tuple([X|Xs]),const(N)),T) :- M is N-1,hastype(Gamma,proj(tuple(Xs),const(M)),T). 

%typeElaborates
typeElaborates(Gamma,sim(variable(X),E),[p(variable(X),T)]) :- hastype(Gamma,E,T).
typeElaborates(Gamma,seq(D1,D2),T4) :- typeElaborates(Gamma,D1,T1),append(T1,Gamma,T2),typeElaborates(T2,D2,T3),append(T3,T1,T4).
typeElaborates(Gamma,par(D1,D2),T) :- \+uc(D1,D2),typeElaborates(Gamma,D1,T1),typeElaborates(Gamma,D2,T2),append(T2,T1,T).
typeElaborates(Gamma,loc(D1,D2),T) :- typeElaborates(Gamma,D1,T1),append(T1,Gamma,T2),typeElaborates(T2,D2,T). 


