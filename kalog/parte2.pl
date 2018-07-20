%digito(?D): base de datos de dígitos del 1 al 9.
digito(1).
digito(2).
digito(3).
digito(4).
digito(5).
digito(6).
digito(7).
digito(8).
digito(9).

%kalogStd(N,T): Resuelve el juego número N mediante prolog estándar y lo presenta en la variable T.
kalogStd(N, T) :- juego(N,T), generarFilas(T), trasponerYChequear(T).

%generarFilas(+T): instancia las variables de las filas de un tablero con valores distintos en cada bloque tal que sumen los valores indicados en el mismo.
generarFilas([]).
generarFilas([F|R]) :- generarFila(F), generarFilas(R).

%generarFila(+F): instancia las variables de una fila con valores distintos en cada bloque tal que sumen los valores indicados en el mismo.
generarFila([]).
generarFila([n|R]) :- generarFila(R).
generarFila([c(_)|R]) :- generarFila(R).
generarFila([f(SF)|R]) :- generarBloqueFila(SF,[],R).
generarFila([p(SF,_)|R]) :- generarFila([f(SF)|R]).

%generarBloqueFila(+SF, +AcF, +F): instancia las variables del bloque actual de F utilizando valores distintos a los de AcF, haciendo que sume en total SF.
generarBloqueFila(0, _, []).
generarBloqueFila(SF, AcF, [X|R]) :- var(X), digito(X), not(member(X,AcF)), SF2 is SF - X, generarBloqueFila(SF2, [X|AcF], R). %noSePasa(X,AcF), 
generarBloqueFila(0, _, [X|R]) :- nonvar(X), generarFila([X|R]).

%trasponerYChequear(+T): trasone un tablero para luego chequearlo con chequearColumnas.
trasponerYChequear(T) :- trasponer(T,T2), chequearColumnas(T2).

%trasponer(+M,?M2): traspone una matriz. Utiliza predicado auxiliar fila.
trasponer([[]|_], []).
trasponer(M, [X|T]) :- fila(M, X, M1), trasponer(M1, T).
fila([], [], []).
fila([[X|Xs]|Ys], [X|R], [Xs|Z]) :- fila(Ys, R, Z).

%chequearColumnas(+Tablero) para cada columna de Tablero, chequea que la suma sea correcta.
chequearColumnas([]).
chequearColumnas([C|R]) :- chequearColumna(C), chequearColumnas(R).

%chequearColumna(+Columna) chequea que Columna, cumpla con la suma correspondiente.
chequearColumna([]).
chequearColumna([n|R]) :- chequearColumna(R).
chequearColumna([f(_)|R]) :- chequearColumna(R).
chequearColumna([c(SC)|R]) :- chequearBloqueColumna(SC,[],R).
chequearColumna([p(_,SC)|R]) :- chequearColumna([c(SC)|R]).

%chequearBloqueColumna(+Suma, +Acc, +Columna) chequea que los elementos de la columna sumen Suma, y sean diferentes entre si.
chequearBloqueColumna(SC, AcC, [X|R]) :- integer(X), not(member(X,AcC)), SC2 is SC - X, chequearBloqueColumna(SC2, [X|AcC], R). %noSePasa(X,AcF), 
chequearBloqueColumna(0, _, C) :- chequearColumna(C).