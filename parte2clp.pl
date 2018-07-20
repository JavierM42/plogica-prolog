:- use_module(library(clpfd)).

kalogClp(T) :- generarFilasCLP(T), trasponerYChequearCLP(T), appendMatrizCLP(T, Array), labelearCLP(Array,[]).

%generarFilasCLP(+T) instancia las variables de las filas de un tablero con valores distintos en cada bloque tal que sumen los valores indicados en el mismo.
generarFilasCLP([]).
generarFilasCLP([F|R]) :- generarFilaCLP(F), generarFilasCLP(R).

%generarFilaCLP(+F): instancia las variables de una fila con valores distintos en cada bloque tal que sumen los valores indicados en el mismo.
generarFilaCLP([]).
generarFilaCLP([n|R]) :- generarFilaCLP(R).
generarFilaCLP([c(_)|R]) :- generarFilaCLP(R).
generarFilaCLP([f(SF)|R]) :- generarBloqueFilaCLP(SF,[],R).
generarFilaCLP([p(SF,_)|R]) :- generarFilaCLP([f(SF)|R]).

%generarBloqueFila(+SF, +AcF, +F): instancia las variables del bloque actual de F utilizando valores distintos a los de AcF, haciendo que sume en total SF.
generarBloqueFilaCLP(SF, AcF, []) :- rellenarBloqueCLP(SF, AcF).
generarBloqueFilaCLP(SF, AcF, [X|R]) :- var(X), generarBloqueFilaCLP(SF, [X|AcF], R).
generarBloqueFilaCLP(SF, AcF, [X|R]) :- nonvar(X), rellenarBloqueCLP(SF, AcF), generarFilaCLP([X|R]).

%rellenarBloqueCLP(+SF,+B): rellena un bloque de la fila con valores utilizando CLP. 
rellenarBloqueCLP(0, []).
rellenarBloqueCLP(SF, B) :- B ins 1..9, all_different(B), sum(B,#=,SF).

%trasponerYChequearCLP(+T): para chequear las columnas, transpone el tablero utilizando el transpose de clp(fd) para luego chequearlo utilizando chequearColumnasCLP.
trasponerYChequearCLP(T) :- transpose(T,T2), chequearColumnasCLP(T2).

%chequearColumnasCLP(+Tablero) para cada columna de Tablero, chequea que las restricciones sean correctas.
chequearColumnasCLP([]).
chequearColumnasCLP([C|R]) :- chequearColumnaCLP(C), chequearColumnasCLP(R).

%chequearColumnaCLP(+Columna) chequea que Columna cumpla con las sumas correspondientes.
chequearColumnaCLP([]).
chequearColumnaCLP([n|R]) :- chequearColumnaCLP(R).
chequearColumnaCLP([f(_)|R]) :- chequearColumnaCLP(R).
chequearColumnaCLP([c(SC)|R]) :- chequearBloqueColumnaCLP(SC,[],R).
chequearColumnaCLP([p(_,SC)|R]) :- chequearColumnaCLP([c(SC)|R]).

%chequearBloqueColumnaCLP(+Suma, +Acc, +Columna) chequea que los elementos del bloque sumen Suma, y sean diferentes entre si.
chequearBloqueColumnaCLP(SC, AcC, [X|R]) :- var(X), chequearBloqueColumnaCLP(SC, [X|AcC], R). 
chequearBloqueColumnaCLP(SC, AcC, [X|R]) :- nonvar(X), integer(X), chequearBloqueColumnaCLP(SC, [X|AcC], R). 
chequearBloqueColumnaCLP(SC, AcC, [X|R]) :- nonvar(X), rellenarBloqueCLP(SC, AcC), chequearColumnaCLP([X|R]).
chequearBloqueColumnaCLP(SC, AcC, []) :- rellenarBloqueCLP(SC, AcC).

%appendMatrizCLP(+T, ?V): convierte una matriz en un vector de sus celdas
appendMatrizCLP([], []).
appendMatrizCLP([F|R], X) :- appendMatriz(R,Y), append(F,Y,X).

%labelarCLP(+T,+Ac): extrae las variables y las labelea en un vector
labelearCLP([],Ac) :- label(Ac).
labelearCLP([X|R], Ac) :- var(X), labelearCLP(R,[X|Ac]).
labelearCLP([X|R], Ac) :- nonvar(X), labelearCLP(R,Ac).
