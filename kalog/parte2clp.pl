:- use_module(library(clpfd)).

kalogClp(T) :- generarFilasCLP(T), trasponerYGenerarColumnasCLP(T), appendMatrizCLP(T, Array), labelearCLP(Array,[]).

%generarFilasCLP(+T) instancia las variables de las filas de un tablero con valores distintos en cada bloque tal que sumen los valores indicados en el mismo.
generarFilasCLP([]).
generarFilasCLP([F|R]) :- generarFilaCLP(F), generarFilasCLP(R).

%generarFilaCLP(+F): instancia las variables de una fila con valores distintos en cada bloque tal que sumen los valores indicados en el mismo.
generarFilaCLP([]).
generarFilaCLP([n|R]) :- generarFilaCLP(R).
generarFilaCLP([c(_)|R]) :- generarFilaCLP(R).
generarFilaCLP([f(SF)|R]) :- generarBloqueFilaCLP(SF,[],R).
generarFilaCLP([p(SF,_)|R]) :- generarFilaCLP([f(SF)|R]).

%generarBloqueFila(+SF, +AcF, +F): instancia las variables del bloque actual de F utiliçzando valores distintos a los de AcF, haciendo que sume en total SF.
generarBloqueFilaCLP(SF, AcF, []) :- rellenarBloqueCLP(SF, AcF).
generarBloqueFilaCLP(SF, AcF, [X|R]) :- var(X), generarBloqueFilaCLP(SF, [X|AcF], R).
generarBloqueFilaCLP(SF, AcF, [X|R]) :- nonvar(X), rellenarBloqueCLP(SF, AcF), generarFilaCLP([X|R]).

%rellenarBloqueCLP(+SF,+B): rellena un bloque de la fila con valores utilizando CLP. 
rellenarBloqueCLP(0, []).
rellenarBloqueCLP(SF, B) :- B ins 1..9, all_different(B), sum(B,#=,SF).

%trasponerYGenerarColumnasCLP(+T): transpone el tablero utilizando el transpose de clp(fd) para luego chequearlo utilizando generarColumnasCLP.
trasponerYGenerarColumnasCLP(T) :- transpose(T,T2), generarColumnasCLP(T2).

%generarColumnasCLP(+Tablero) para cada columna de Tablero, chequea que la suma sea correcta.
generarColumnasCLP([]).
generarColumnasCLP([C|R]) :- generarColumnaCLP(C), generarColumnasCLP(R).

%generarColumnaCLP(+Columna) chequea que Columna, cumpla con la suma correspondiente.
generarColumnaCLP([]).
generarColumnaCLP([n|R]) :- generarColumnaCLP(R).
generarColumnaCLP([f(_)|R]) :- generarColumnaCLP(R).
generarColumnaCLP([c(SC)|R]) :- generarBloqueColumnaCLP(SC,[],R).
generarColumnaCLP([p(_,SC)|R]) :- generarColumnaCLP([c(SC)|R]).

%generarBloqueColumnaCLP(+Suma, +Acc, +Columna) chequea que los elementos de la columna sumen Suma, y sean diferentes entre si.
generarBloqueColumnaCLP(SC, AcC, [X|R]) :- var(X), generarBloqueColumnaCLP(SC, [X|AcC], R). 
generarBloqueColumnaCLP(SC, AcC, [X|R]) :- nonvar(X), integer(X), generarBloqueColumnaCLP(SC, [X|AcC], R). 
generarBloqueColumnaCLP(SC, AcC, [X|R]) :- nonvar(X), rellenarBloqueCLP(SC, AcC), generarColumnaCLP([X|R]).
generarBloqueColumnaCLP(SC, AcC, []) :- rellenarBloqueCLP(SC, AcC).

%appendMatrizCLP(+T, ?V) convierte una matriz en un vector de sus celdas
appendMatrizCLP([], []).
appendMatrizCLP([F|R], X) :- appendMatriz(R,Y), append(F,Y,X).

%labelearCLP(+V, +Ac) para cada variable en el vector V, labelea con clpf, dando un solo resultado, en orden aleatorio.
labelearCLP([],Ac) :- label(Ac).
labelearCLP([X|R], Ac) :- var(X), labelearCLP(R,[X|Ac]).
labelearCLP([X|R], Ac) :- nonvar(X), labelearCLP(R,Ac).
