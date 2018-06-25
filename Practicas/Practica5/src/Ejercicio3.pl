%El mundo de los bloques.

bloques(X) :- X=a; X=b; X=c.
mundo(X) :- X=a; X=b; X=c; X=m1; X=m2;X=m3.

%Del estado s0, mover X sobre Y.
poss(mover(X,Y),S) :- bloques(X),mundo(Y),
\+ X=Y,
\+ on(Z,X,S),
\+ on(Z,Y,S).

on(X,Y,do(A,S)) :- poss(A,S),on(X,Y,S),
\+ A=mover(X,Z).

on(X,Y,do(A,S)) :- poss(A,S),A=mover(X,Y).

%Estado actual de los bloques.
on(a,m1,s0).
on(b,a,s0).
on(c,b,s0).


