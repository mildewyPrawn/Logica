%telefono(a).
%telefono(b).
%telefono(c).
%telefono(d).
%telefono(e).
%telefono(f).
telefono(g).

arcos([a-b,b-c,b-e,c-d,d-e,e-f,e-g]).

adyacente(X,Y) :-
    arcos(L),
    (member(X-Y,L);
     member(Y-X,L)).

camino(A,Z,C) :-
    camino_aux(A,[Z],C).

camino_aux(A,[A|C1],[A|C1]).

camino_aux(A,[Y|C1],C) :-
    adyacente(X,Y),
    not(member(X,[Y|C1])),
    camino_aux(A,[X,Y|C1],C).

caminoCorto(A,Z,C) :-
    camino_aux(A,[Z],C),
    !.
