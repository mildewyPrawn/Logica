bt(void).
bt(node(A, T1, T2)) :-
    integer(A),
    bt(T1),
    bt(T2).

/* elem(A,T) se cumple si A es elemento de T.*/
elem(X, bt(node(A, T1, T2))) :-
    X=A;
    elem(X, T1);
    elem(X, T2).

/* maxtree(A,T) se cumple si A es mayor que todos los elementos de T*/
maxtree(_,void).
maxtree(X, bt(node(A, T1, T2))) :-
    X>A,
    maxtree(A, T1),
    maxtree(A, T2).

/* mintree(A,T) se cumple si A es menor que todos los elementos de T*/
mintree(_,void).
mintree(X, bt(node(A, T1, T2))) :-
    X<A,
    mintree(A, T1),
    mintree(A, T2).

/* innode(A,T) se cumple si A está en T.*/
innode(X, bt(node(A, T1, T2))) :-
    X=A;
    innode(X, T1);
    innode(X, T2).
