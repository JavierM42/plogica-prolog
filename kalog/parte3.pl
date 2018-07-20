:- use_module(library(random)).
:- use_module(library(clpfd)).

% generar(+F,+C,-TFinal) procedimiento principal que genera tablero de F filas y C columnas.
generar(F,C,TFinal) :- matriz(F, C, T),
	bloques(T),
	restricciones(T),
	appendMatriz(T,Array),
	labelear(Array,[]),
	simplificar(T,TFinal).

% largoFilas(?A, ?N) todas las filas de la matriz A tienen largo N.
largoFilas([],_).
largoFilas([F|R],N) :- length(F,N), largoFilas(R,N).

% matriz(+M,+N,?A) A es una matriz de variables de MxN.
matriz(M,N,A) :- length(A,M), largoFilas(A,N).

% bloques(+M) M es una matriz de variables. la primer fila y columna de M se instancian con p(_,_), así como elemnentos aleatorios de la matriz con probabilidad 1/10.
bloques([F|R]):- bloquesFila1(F), bloquesResto(R).
bloquesFila1([]).
bloquesFila1([p(_,_)|R]) :- bloquesFila1(R).
bloquesResto([]).
bloquesResto([F|R]) :- bloquesFila(F), bloquesResto(R).
bloquesFila([p(_,_)|R]) :- bloquesFilaResto(R).
bloquesFilaResto([]).
bloquesFilaResto([X|R]) :- bloqueConProbabilidad(X), bloquesFilaResto(R).
bloqueConProbabilidad(X) :- random_member(X, [ p(_,_), _, _, _, _, _, _, _, _ ]).

% restricciones(+T) para cada p(_,_) en la matriz, establece restricciones clpfd para las variables adyacentes en sus fila y columnas.
restricciones(T) :- restriccionesFilas(T), transpose(T,T2), restriccionesColumnas(T2).

% restriccionesFilas(+T) para cada fila, establece restricciones clpfd para las variables.
restriccionesFilas([]).
restriccionesFilas([F|R]) :- restriccionesFila(F), restriccionesFilas(R).

% restriccionesFila(+F) para cada p(SF,_) en la fila, establece restricciones clpfd para las variables adyacentes a derecha, tal que todas sean dígitos distintos que sumen el valor SF.
restriccionesFila([]).
restriccionesFila([X|R]) :- var(X), restriccionesFila(R), !.
restriccionesFila([p(SF,_)|R]) :- restriccionesBloque(SF, [], R), restriccionesFila(R).

% restriccionesBloque(+SF, +AC, +F) Para los elementos iniciales de F que sean variables, establece las restricciones descritas en restriccionesFila.
restriccionesBloque(SF, Ac, []):- Ac ins 1..9, all_distinct(Ac), sum(Ac,#=,SF), !.
restriccionesBloque(SF, Ac, [X|R]) :- var(X), restriccionesBloque(SF, [X|Ac], R), !.
restriccionesBloque(SF, Ac, [X|_]) :- nonvar(X), restriccionesBloque(SF,Ac,[]).

% restriccionesColumnas(+T) para cada columna (fila porque la matriz es transpuesta), establece restricciones clpfd para las variables.
restriccionesColumnas([]).
restriccionesColumnas([F|R]) :- restriccionesColumna(F), restriccionesColumnas(R).

% restriccionesColumna(C) ppara cada p(_,SC) en la columna, establece restricciones clpfd para las variables adyacentes a derecha, tal que todas sean dígitos distintos que sumen el valor SC.
restriccionesColumna([]).
restriccionesColumna([X|R]) :- var(X), restriccionesColumna(R), !.
restriccionesColumna([p(_,SC)|R]) :- restriccionesBloque(SC, [], R), restriccionesColumna(R).

%appendMatriz(+T, ?V) convierte una matriz en un vector de sus celdas
appendMatriz([], []).
appendMatriz([F|R], X) :- appendMatriz(R,Y), append(F,Y,X).

% labelear(+V, +Ac) para cada variable en el vector V, labelea con clpf, dando un solo resultado, en orden aleatorio.
labelear([],Ac) :- random_between(0,10000,Seed), labeling([random_variable(Seed), random_value(Seed)], Ac).
labelear([X|R], Ac) :- var(X), labelear(R,[X|Ac]), !.
labelear([p(X,Y)|R], Ac) :- labelear(R,[X,Y|Ac]).

% simplificar(+T,?T2) T2 es T, a excepción que los p(0,0) son n, los p(X,0) son f(X), y los p(0,X) son c(X).
simplificar([],[]).
simplificar([F|R], [F2|R2]) :- simplificarFila(F,F2), simplificar(R,R2).
simplificarFila([], []).
simplificarFila([X|R], [X|R2]) :- integer(X), simplificarFila(R,R2), !.
simplificarFila([p(0,0)|R], [n|R2]) :- simplificarFila(R,R2), !.
simplificarFila([p(0,SC)|R], [c(SC)|R2]) :- simplificarFila(R,R2), !.
simplificarFila([p(SF,0)|R], [f(SF)|R2]) :- simplificarFila(R,R2), !.
simplificarFila([X|R], [X|R2]) :- simplificarFila(R,R2).








