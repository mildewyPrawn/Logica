% Autómata

estado(q0,b,q1).
estado(q1,a,q1).
estado(q1,a,q2).
estado(q2,b,q1).

inicial(q0).
final(q2).

% Predicado que dada una lista l y un estado 'q', es verdadero si
% a partir de 'q' se puede llegar al estado q2, leyendo la cadena 'l'.

reconoce(Q, [A|B]) :-
    estado(Q,A,QN),
    reconoce(QN, B).

reconoce(Q, []) :-
    final(Q).

% Nos dice si la cadena 'baba' es aceptada por el autómata, parte del
% estado q0.

baba(q0) :-
    reconoce(q0, [b,a,b,a]).
