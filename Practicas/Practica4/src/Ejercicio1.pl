and(0,0,0).
and(0,1,0).
and(1,0,0).
and(1,1,1).

or(0,0,0).
or(0,1,1).
or(1,0,1).
or(1,1,1).

not(0,1).
not(1,0).

circa(A,B,Z):-
    not(A,X),
    and(X,B,Y),
    not(Y,R),
    and(R,B,Z).

circb(A,B,C,Z):-
    not(A,X),
    and(A,X,Y),
    not(Y,V),
    and(B,C,W),
    and(V,W,Z).
