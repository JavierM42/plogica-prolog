:- module(lab1,[largo/2, largoAc/3, ultimo/2, penultimo/2, diferentes/1, diferentes/2, simplificada/2, empaquetada/2, comprimida/2, descomprimida/2, matrizFija/4, valor_celda/4, nuevo_valor_celda/5, cartesiano/3, composicion/3, latino/3, cuadrado_GL/4]).


/*largo(+L,?N) N es el largo de la lista L.*/
largo([],0).
largo([_|L],N):- largo(L,N2), N is (N2+1).

/*largoAc(+L,+Ac,?N) N  es el largo de la lista L, el predicado es tail-recursive*/
largoAc([],N,N).
largoAc([_|L],Ac,N) :- Ac2 is (Ac+1), largoAc(L,Ac2,N).

/*ultimo(?L,?U) el último elemento de la lista L unifica con U.*/
ultimo([X],X).
ultimo([_|L],U) :- ultimo(L,U).

/*penultimo(?L,?U) el penúltimo elemento de la lista L unifica con U.*/
penultimo([H|[_]],H).
penultimo([_|L],P) :- penultimo(L,P).

/*diferentes(+L) todos los elementos de la lista L son distintos entre sí */
/*auxiliar: notmember(+H, +L) El elemento H no pertenece a L.*/
notmember(_,[]).
notmember(X, [H|L]) :- X\=H, notmember(X,L).

diferentes([]).
diferentes([H|L]) :- notmember(H,L), diferentes(L).

/*diferentes (+L1,+L2) L1 y L2 tiene el mismo largo y todos los elementos correspondientes son distintos*/
diferentes([],[]).
diferentes([H1|L1],[H2|L2]) :- (H1\=H2), diferentes(L1,L2).

/*simplificada(+L1,?L2) L2 contiene los mismo elementos que L1 y en el mismo orden, pero con
una sola instancia de elementos consecutivos repetidos en L1*/
simplificada([],[]).
simplificada([H],[H]).
simplificada([H|[H|L1]],[H|L2]) :- simplificada([H|L1],[H|L2]).
simplificada([H|[H2|L1]],[H|L2]):- (H\=H2), simplificada([H2|L1],L2).

/*empaquetada(+L1,?L2) L2 contiene listas con los elementos consecutivos repetidos (ocurren 1
o más veces consecutivas) de L1 , en el mismo orden*/
empaquetada([],[]).
empaquetada([H],[[H]]).
empaquetada([H|[H|L]], [[H|Hs]|L2]) :- empaquetada([H|L],[Hs|L2]).
empaquetada([H|[K|L]], [[H]|L2]) :- (H\=K), empaquetada([K|L],L2).

/*comprimida(+L1,?L2) L2 contiene  pares formados por los elementos de L1, en el mismo orden
y la cantidad de veces consecutivas que ocurren*/
comprimida([],[]).
comprimida([H],[p(H,1)]).
comprimida([H|[H|L]], [p(H,N)|L2]) :- comprimida([H|L],[p(H,M)|L2]), N is (M+1).
comprimida([H|[K|L]], [p(H,1)|L2]) :- (H\=K), comprimida([K|L],L2).


/*Descomprimida(?L1,+L2) L2 contiene   pares formados por los elementos de L1, en el mismo
orden y la cantidad de veces consecutivas que ocurren*/
/*asumimos que los ? y + son al revés, porque si no no tiene sentido, es llamar a comprimida con los argumentos intercambiados.*/
descomprimida([],[]).
descomprimida([H|L], [p(H,N)|L2]) :- (N =\= 0), M is (N-1), descomprimida(L,[p(H,M)|L2]).
descomprimida(L, [p(_,0)|L2]) :- descomprimida(L,L2).

/*matrizFija(?M,?N,+E,?A)  A es una matriz  de  M filas  y N columnas.  Cada celda debe  tener  el
valor E. La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas*/

/*auxiliar: largoFilas(?A, ?N) todas las filas de la matriz A tienen largo N*/
largoFilas([],_).
largoFilas([F|R],N) :- length(F,N), largoFilas(R,N).

/*auxiliar: contenidoFila(?L, ?E) todos los elementos de la lista L son E*/
contenidoFila([],_).
contenidoFila([H|L],H) :- contenidoFila(L,H).

/*auxiliar: contenidoMatriz(?A, ?E) todos los elementos de la matriz A son E*/
contenidoMatriz([], _).
contenidoMatriz([F|R],E) :- contenidoFila(F,E), contenidoMatriz(R,E).

matrizFija(M,N,E,A) :- length(A,M), largoFilas(A,N), contenidoMatriz(A,E).

/*valor_celda(+I,+J,+A,?E) E es el contenido de la celda (I,J) de la matriz A*/
valor_celda(1,1,[[E|_]|_],E).
valor_celda(1,J,[[_|F]|R],E) :- J2 is (J-1), valor_celda(1,J2,[F|R],E).
valor_celda(I,J,[_|R],E) :- (I2 is I-1), valor_celda(I2,J,R,E).

/*nuevo_valor_celda(+I,+J,+A1,+E,?A2) A2   es   una   matriz   que   contiene   el   valor   E   en   la   celda
(I,J) y en el resto de las celdas contiene los mismos valores que A1*/
nuevo_valor_celda(1,1,[[_|F]|R],E,[[E|F]|R]).
nuevo_valor_celda(1,J,[[H|F]|R],E,[[H|F2]|R]) :- J2 is (J-1), nuevo_valor_celda(1,J2,[F],E,[F2]).
nuevo_valor_celda(I,J,[F|R],E,[F|R2]) :- I2 is (I-1), nuevo_valor_celda(I2,J,R,E,R2).

/*cartesiano(+V1,+V2,?M) V1 y V2 son vectores de igual dimensión N, M es una matriz NxN de
pares (V1i,V2j) como elemento fila i y columna j de la matriz, siendo V1i el i-ésimo elemento de V1 y V2j el j-ésimo elemento de V2*/

/*auxiliar: aparear(+E, +L, ?R) R es un vector de pares donde el primer elemento de cada par es E y el segundo es el elemento con el mismo índice en L.*/
aparear(A,[B],[(A,B)]).
aparear(A,[B|R],[(A,B)|R2]) :- aparear(A,R,R2).

/*auxiliar: cartesianoMN(+V1, +V2, M) realiza el producto cartesiano en vectores de cualquier tamaño.*/
cartesianoMN([A1],B,[R]) :- aparear(A1,B,R).
cartesianoMN([A|Ra],[B|Rb],[X|Y]) :- aparear(A, [B|Rb],X), cartesianoMN(Ra,[B|Rb],Y).

cartesiano(A,B,R) :- largo(A,L), largo(B,L), cartesianoMN(A,B,R).

/*composicion(+M1,+M2,?M) M es la composición elemento a elemento con el el operador '+' de las matrices de igual dimensión M1 y M2*/
composicionFila([A],[B],[A+B]).
composicionFila([A|Ra],[B|Rb],[A+B|X]) :- composicionFila(Ra,Rb,X).
composicion([A],[B],[X]) :- composicionFila(A,B,X).
composicion([A|Ra],[B|Rb],[X|Y]) :- composicionFila(A,B,X), composicion(Ra,Rb,Y).

/*latino(+N,+E,?Lat) Lat es un cuadrado latino de orden N sobre el conjunto de elementos E*/

/*auxiliar: takeout(+X,?V,?R) V es el vector R con una ocurrencia de X insertada en alguna de sus posiciones.*/
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

/*auxiliar: permutacion(+V,?P) P es una permutacion del vector V*/
permutacion([X|Y],Z) :- permutacion(Y,W), takeout(X,Z,W).
permutacion([],[]).

/*auxiliar: trasponerVector(+V,?M) M es la matriz con filas de un elemento, cada una con un elemento de V en el mismo orden.*/
trasponerVector([A],[[A]]).
trasponerVector([A|R],[[A]|R2]) :- trasponerVector(R,R2).

/*auxiliar: concatenarColumnas(+M1,+M2,?R) R es una matriz con las columnas de M1 y M2 en ese orden*/
concatenarColumnas([[A]],[F],[[A|F]]).
concatenarColumnas([[A]|C],[F|R],[[A|F]|X]):- concatenarColumnas(C,R,X).

/*auxiliar: matrizPermutaciones(+N,+E,?M) M es una matriz de N columnas cuyas columnas son permutaciones de E*/
matrizPermutaciones(1,E,X) :- permutacion(E,P), trasponerVector(P,X).
matrizPermutaciones(N,E,X) :- permutacion(E,P), trasponerVector(P,P2),
    N\=1, N2 is (N-1), matrizPermutaciones(N2,E,Y),
    concatenarColumnas(P2,Y,X).

/*auxiliar: chequearFilas(+C) cada fila de la matriz C no tiene elementos repetidos en la fila.*/
chequearFilas([F]) :- diferentes(F).
chequearFilas([F|R]) :- diferentes(F), chequearFilas(R).

latino(N,E,C) :- matrizPermutaciones(N,E,C), chequearFilas(C).


/*cuadrado_GL(+N,+L1,+L2,?C) C es un cuadrado grecolatino de orden N, siendo L1 y L2  los elementos de  S y de T  respectivamente*/

/*auxiliar: concatenar(+M,?V) V es un vector con los elementos de las celdas de la matriz M.*/
concatenar([A],A).
concatenar([A|F],X) :- concatenar(F,Y), append(A,Y,X).

cuadrado_GL(N,L1,L2,C) :- latino(N,L1,C1), latino(N,L2,C2), composicion(C1,C2,C), concatenar(C,F), diferentes(F).


