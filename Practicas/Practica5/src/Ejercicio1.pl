% Predicado que agrega un elemento a una lista.
% Donde L = [x|l].

push(X,Lista,L) :-
    L = [X|Lista].

% Predicado que elimina el primer elemento de una lista.
% Donde L = l.

pop([_|B], L) :-
    L = B.

% Predicado que hace la concatenación de dos listas.
% Donde L = l1++l2.

append([], L, L).
append([X|L1], L2, [X|L3]) :-
    append(L1, L2, L3).

% Predicado que calcula la suma de los elementos en una lista.
% Donde S es la suma de los elementos de l.

listSum([],0).
listSum([A|B],S) :-
    listSum(B, T),
    S is T + 1.
