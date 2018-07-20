:- use_module(graficos).

:- pce_image_directory('./').
:- consult('parte2.pl').
:- consult('parte2clp.pl').
:- consult('parte3.pl').

% ======================================================
% juegos predefinidos
% ======================================================

juego(1,[
[n,c(9),c(34),c(4),n],
[f(9),_,_,_,n],
[f(13),_,_,_,n],
[f(13),_,_,c(11),c(3)],
[n,f(7),_,_,_],
[n,f(19),_,_,_]
]).

juego(2,[
[n,n,n,c(7),c(16)],
[n,n,p(13,6),_,_],
[n,p(11,4),_,_,_],
[f(6),_,_,_,n],
[f(3),_,_,n,n]
]).

juego(3,[
[n,c(9),c(24),n,c(13),c(14),n,n],
[f(4),_,_,f(14),_,_,n,n],
[f(14),_,_,p(5,11),_,_,c(31),n],
[n,p(15,9),_,_,p(11,7),_,_,c(6)],
[f(22),_,_,_,_,p(13,13),_,_],
[f(3),_,_,p(15,15),_,_,_,_],
[n,f(3),_,_,p(11,11),_,_,c(15)],
[n,n,f(8),_,_,f(8),_,_],
[n,n,f(16),_,_,f(16),_,_]
]).

% más juegos
juego(4,[
[n,n,c(29),c(10),n],
[n,p(6,4),_,_,c(16)],
[f(18),_,_,_,_],
[f(23),_,_,_,_],
[n,f(12),_,_,n]
]).

juego(5,[
[n,c(12),c(26),c(14),n],
[f(22),_,_,_,n],
[f(13),_,_,_,c(12)],
[n,f(22),_,_,_],
[n,f(7),_,_,_]
]).

juego(6,[
[n,n,n,c(18),c(4),n,n,n],
[n,n,p(12,21),_,_,c(17),n,n],
[n,f(14),_,_,_,_,c(28),n],
[n,p(15,16),_,_,f(10),_,_,c(12)],
[f(9),_,_,n,n,f(14),_,_],
[f(16),_,_,c(9),n,p(11,12),_,_],
[n,f(8),_,_,p(10,5),_,_,n],
[n,n,f(11),_,_,_,_,n],
[n,n,n,f(12),_,_,n,n]
]).

% ======================================================
% lógica principal de juego
% ======================================================

% kalog(+Tablero) <- despliega un Tablero y permite que el usuario humano lo complete.
kalog(Tablero) :-
    tam_tablero(Tablero,F,C),
    gr_crear_tablero(F, C, [boton('Reiniciar',reiniciar),boton('Resolver',resolver), boton('Salir',salir)], Visual),
	dibujarCasillas(1,Tablero,Visual),
    loop(Visual,Tablero,e(none, none, Tablero)),
    !,
    gr_destruir(Visual).

% kalog(+NroJuego,+Tecnica) <- resuelve el kakuro definido en Nro Juego con la técnica Tecnica.
% Tecnica puede tener los valores std o clpfd.

kalog(N,std) :- kalogStd(N, _).
kalog(N,clpfd) :- juego(N,T), kalogClp(T).

% kalog(+Filas,+Columnas,-Tablero) <- genera un kakuro de tamaño (Filas, Columnas).
kalog(F,C,T) :- generar(F,C,T).

% loop principal del juego, el estado guarda información sobre lo que el usuario está haciendo,
% en principio el estado es e(CasilleroSeleccionado,NumeroSeleccionado)
% !! El estado es e(casilleroSeleccionado,NumeroSeleccionado,Tablero) donde tablero es el tablero actual (con números puestos donde el usuario ha puesto números). Las demás instancias de "Tablero" se mantienen fijas con el tablero inicial.
% CasilleroSeleccionado puede ser none o un par (Fila,Columna) que indica la posición en el tablero
% NumeroSeleccionado puede ser none o una tupla (Fila,Columna,Num) que indica la posición y el valor en la lista de números
loop(Visual,Tablero,Estado) :-
    gr_evento(Visual,E),
    procesar_evento(E,Visual,Tablero,Estado,NuevoEstado),
    !,
    loop(Visual,Tablero,NuevoEstado).
loop(_,_,_).

% botón salir
procesar_evento(salir,Visual,_,Estado,Estado) :-
 !, gr_opciones(Visual, 'Seguro?', ['Si', 'No'], 'No').

% botón reiniciar
procesar_evento(reiniciar, Visual, Tablero, _ , e(none,none,Tablero)) :-
 !,
 tam_tablero(Tablero,F,C),
 gr_inicializar_tablero(Visual,F,C),
 dibujarCasillas(1,Tablero,Visual).


% boton resolver
procesar_evento(resolver, Visual, Tablero, _, e(none,none,T2)) :- copiarTablero(Tablero,T2), kalogClp(T2), dibujarCasillas(1,T2,Visual), gr_estado(Visual,"Tablero resuelto automáticamente").

% el evento es un click -> actualizo mensaje y proceso.
procesar_evento(click(Fila,Columna),Visual,T,Estado,NuevoEstado) :-
    sformat(Msg, 'Click en (~w, ~w)', [Fila,Columna]),
    gr_estado(Visual,Msg),
    procesar_click(Fila,Columna,Visual,T,Estado,NuevoEstado).


% click en una casilla, sin número seleccionado
procesar_click(FilaC,ColC,Visual,Tablero,e(CasViejo,none,T),e((FilaC, ColC),none,T)):-
    casillero_valido(FilaC, ColC, Tablero),
    !,
    desmarcar(Visual, CasViejo),
    gr_marcar_seleccion(Visual, FilaC, ColC).

% click en una casilla, con número seleccionado
procesar_click(FilaC, ColC, Visual,Tablero,e(none,(FNum,CNum,N),T),e(none,none,T2)):-
    casillero_valido(FilaC, ColC, Tablero),
    desmarcar(Visual, (FNum, CNum)),
	asignarEstado(T,T2,N,FilaC,ColC),
	validarFila(FilaC, T2),
	transpose(T2,T3),
	validarColumna(ColC, T3),
    procesar_asignar_numero(Visual,FilaC, ColC, N), !.
procesar_click(FilaC, ColC, Visual,Tablero,e(none,(FNum,CNum,N),T),e(none,none,T2)):-
    casillero_valido(FilaC, ColC, Tablero),
    !,
    desmarcar(Visual, (FNum, CNum)),
	asignarEstado(T,T2,N,FilaC,ColC),
    procesar_asignar_numero(Visual,FilaC, ColC, N),
	gr_marcar_error(Visual,FilaC,ColC).

% click en número sin casilla seleccionada.
procesar_click(FilaN,ColN,Visual,Tablero,e(none,NumOld,T),e(none,(FilaN,ColN,N),T)):-
    numero_valido(FilaN, ColN, Tablero, N),
    !,
    desmarcar(Visual, NumOld),
    gr_marcar_seleccion(Visual, FilaN, ColN).

% click en número con casilla seleccionada.
procesar_click(FilaN, ColN, Visual, Tablero, e((FilaC,ColC),none,T),e(none,none,T2)):-
    numero_valido(FilaN, ColN, Tablero, N),
    desmarcar(Visual, (FilaC, ColC)),
	asignarEstado(T,T2,N,FilaC,ColC),
	validarFila(FilaC, T2),
	transpose(T2,T3),
	validarColumna(ColC, T3),
    procesar_asignar_numero(Visual, FilaC, ColC, N), !.
procesar_click(FilaN, ColN, Visual, Tablero, e((FilaC,ColC),none,T),e(none,none,T2)):-
    numero_valido(FilaN, ColN, Tablero, N),
    !,
    desmarcar(Visual, (FilaC, ColC)),
	asignarEstado(T,T2,N,FilaC,ColC),
    procesar_asignar_numero(Visual, FilaC, ColC, N),
	gr_marcar_error(Visual,FilaC,ColC).


% click en cualquier otro lado -> ignorar.
procesar_click(_,_,_,_,Estado,Estado).

procesar_asignar_numero(Visual, FilaC, ColC, x):-
    !,
    gr_eliminar_numero(Visual, FilaC, ColC).
	
procesar_asignar_numero(Visual, FilaC, ColC, N):-
    between(1,9,N),
    gr_dibujar_numero(Visual, FilaC, ColC, N).
%quita la marca de seleccionado de casillas y números.
desmarcar(Visual, (Fila,Columna,_)):-
    !,
    gr_desmarcar_seleccion(Visual, Fila, Columna).
desmarcar(Visual, (Fila,Columna)):-
    !,
    gr_desmarcar_seleccion(Visual, Fila, Columna).
desmarcar(_,_).

% ======================================================
% predicados auxiliares
% ======================================================

%determina si (Fila,Columna) corresponde a un casillero para llenar.
casillero_valido(Fila,Columna,Tablero):-
    tam_tablero(Tablero,MaxF,MaxC),
    between(1,MaxF,Fila),
    between(1,MaxC,Columna),
	chequearValidez(Fila,Columna,Tablero).

%chequearValidez(+NumC, +NumF, +T) : chequea si la casilla corresponde a un casillero para llenar (blanco)	
chequearValidez(NumF,NumC,[_|R]) :- not(NumF is 1), NumF2 is NumF-1, chequearValidez(NumF2,NumC,R).
chequearValidez(1,NumC,[[_|F]|R]) :- not(NumC is 1), NumC2 is NumC-1, chequearValidez(1,NumC2,[F|R]).
chequearValidez(1,1,[[X|_]|_]) :- var(X).
chequearValidez(1,1,[[X|_]|_]) :- integer(X).

%dibujarCasillas(+NumF,+T,+Visual) : dibuja el tablero en pantalla
dibujarCasillas(_,[],_).
dibujarCasillas(NumF, [F|R],Visual) :- dibujarCasillasFila(NumF, 1, F,Visual), NumF2 is NumF+1, dibujarCasillas(NumF2, R,Visual).

%dibujarCasillasFila(+NumF, +NumC, +F ,+Visual) : dibuja el tableto en pantalla por filas
dibujarCasillasFila(_,_,[],_).
dibujarCasillasFila(NumF, NumC, [X|R], Visual) :- var(X), gr_eliminar_numero(Visual,NumF,NumC), NumC2 is NumC+1, dibujarCasillasFila(NumF, NumC2, R, Visual), !.
dibujarCasillasFila(NumF, NumC, [n|R], Visual) :- gr_dibujar_casillero(Visual,NumF,NumC,0,0), NumC2 is NumC+1, dibujarCasillasFila(NumF, NumC2, R, Visual).
dibujarCasillasFila(NumF, NumC, [f(SF)|R], Visual) :- gr_dibujar_casillero(Visual,NumF,NumC,SF,0), NumC2 is NumC+1, dibujarCasillasFila(NumF, NumC2, R, Visual).
dibujarCasillasFila(NumF, NumC, [c(SC)|R], Visual) :- gr_dibujar_casillero(Visual,NumF,NumC,0,SC), NumC2 is NumC+1, dibujarCasillasFila(NumF, NumC2, R, Visual).
dibujarCasillasFila(NumF, NumC, [p(SF,SC)|R], Visual) :- gr_dibujar_casillero(Visual,NumF,NumC,SF,SC), NumC2 is NumC+1, dibujarCasillasFila(NumF, NumC2, R, Visual).
dibujarCasillasFila(NumF, NumC, [X|R], Visual) :- integer(X), gr_dibujar_numero(Visual,NumF,NumC,X), NumC2 is NumC+1, dibujarCasillasFila(NumF, NumC2, R, Visual).

%copiarTablero(+T,+T2) : realiza una copia del tablero T en T2
copiarTablero([],[]).
copiarTablero([F|R], [F2|R2]) :- copiarFila(F,F2), copiarTablero(R,R2).
copiarFila([],[]).
copiarFila([X|R],[X|R2]) :- nonvar(X), copiarFila(R,R2).
copiarFila([X|R],[_|R2]) :- var(X), copiarFila(R,R2).

%determina si (Fila,Columna) corresponde a un número.
numero_valido(Fila, Columna, Tablero, N):-
    tam_tablero(Tablero, MaxF, _),
    Fila is MaxF+2,
    between(1,10,Columna),
    valor_columna(Columna,N).

%devuelve el valor de la columna, o x para eliminar un valor.
valor_columna(N,N):-
    N =< 9.
valor_columna(10,x).

% obtiene el tamaño de un tablero representado como lista de listas
tam_tablero([Fila|Filas],F,C):-
    length([Filas|Filas],F),
    length(Fila,C).

%asginarEstado(+T, +T2, +Valor, +Fila, +Col) : 	devuelve T pero en (Fila, Columna) el valor de T2
asignarEstado([[_|F]|R] ,[[N|F]|R] , N, 1, 1) :- integer(N), !.
asignarEstado([[_|F]|R] ,[[_|F]|R] , x, 1, 1) :- !.
asignarEstado([[X|F]|R] ,[[X|F2]|R2] , N, 1, Col) :- Col2 is Col-1, asignarEstado([F|R],[F2|R2],N,1,Col2), !.
asignarEstado([F|R] ,[F|R2] , N, Fila, Col) :- Fila2 is Fila-1, asignarEstado(R,R2,N,Fila2,Col).

%validaFila(+FilaN, +Tablero) : valida los bloques de la fila que cumplan con las restricciones. 
validarFila(1, [[]|_]).
validarFila(1, [[n|F]|_]) :- validarFila(1, [F]).
validarFila(1, [[c(_)|F]|_]) :- validarFila(1, [F|_]).
validarFila(1, [[p(SF,_)|F]|_]) :- validarFila(1, [[f(SF)|F]|_]).
validarFila(1, [[f(SF)|F]|_]) :- validarBloqueFila(SF, [], F).
validarFila(N, [_|R]) :- not(N is 1), N2 is N-1, validarFila(N2,R).

%validarBloqueFila(+SF, +Ac, +Fila) : valida que el bloque que cumpla con las restricciones
validarBloqueFila(SF, Ac, [X|F]) :- var(X), !, validarBloqueFila(SF, Ac, F).
validarBloqueFila(SF, Ac, [X|F]) :- integer(X), X in 1..9, !, validarBloqueFila(SF, [X|Ac], F).
validarBloqueFila(SF, Ac, F) :- all_different(Ac), sum(Ac,#=<,SF), validarFila(1, [F]).

%validarBloqueColumna(+SF, +Ac, +Fila) : valida que el bloque que cumpla con las restricciones
validarBloqueColumna(SF, Ac, [X|F]) :- var(X), !, validarBloqueColumna(SF, Ac, F).
validarBloqueColumna(SF, Ac, [X|F]) :- integer(X), X in 1..9, !, validarBloqueColumna(SF, [X|Ac], F).
validarBloqueColumna(SF, Ac, F) :- all_different(Ac), sum(Ac,#=<,SF), validarColumna(1, [F]).

%validarColumna(+ColN, +Tablero) : valida que los bloques de la columna cumplan con las restricciones
validarColumna(1, [[]|_]).
validarColumna(1, [[n|F]|_]) :- validarColumna(1, [F]).
validarColumna(1, [[f(_)|F]|_]) :- validarColumna(1, [F|_]).
validarColumna(1, [[p(_,SC)|F]|_]) :- validarColumna(1, [[c(SC)|F]|_]).
validarColumna(1, [[c(SC)|F]|_]) :- validarBloqueColumna(SC, [], F).
validarColumna(N, [_|R]) :- not(N is 1), N2 is N-1, validarColumna(N2,R).
