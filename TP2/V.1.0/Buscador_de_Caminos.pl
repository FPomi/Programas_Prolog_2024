%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1

%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,0,[]).
tablero(F,C,T) :- findall(Fila, (between(1, F, _), crearFila(C, Fila)), T), F > 0, C > 0.

% crearFila(+longitud, ?fila).
crearFila(L,F) :- length(F,L).

%% Ejercicio 2
%% ocupar(+Pos, ?Tablero)
ocupar(pos(F,C),T) :- nonvar(T), nth0(F,T,X), nth0(C,X,ocupada).
ocupar(pos(F,C),T) :- var(T), F1 is F+1, C1 is C+1, tablero(F1,C1,T1), ocupar(pos(F,C),T1).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

vecino(pos(F, C), T, pos(FV, CV)) :-
  ((FV is F+1, CV is C);
  (FV is F-1, CV is C);
  (FV is F, CV is C+1);
  (FV is F, CV is C-1)),
  estaEnRango(pos(FV,CV), T).

% estaEnRango(+Pos, +Tablero)
estaEnRango(pos(F,C), T) :-
  length(T,FilasTablero),
  nth0(0, T, Fila), length(Fila, ColumnasTablero),
  F >= 0, F < FilasTablero,
  C >= 0, C < ColumnasTablero.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
% Para este ej usamos el esquema Generate and Test, vecino/3 genera todas las posiciones candidatas a solución
% y posLibre/2 testea que la posición no esté ocupada.
vecinoLibre(pos(F, C), T, pos(FV, CV)) :-
  vecino(pos(F, C), T, pos(FV, CV)),
  posLibre(pos(FV, CV), T).

% posLibre(+Pos, +Tablero)
posLibre(pos(F, C), T) :-
  nth0(F, T, Fila),
  nth0(C, Fila, Celda),
  not(Celda == ocupada).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

%% camino(+Inicio, +Fin, +Tablero, -Camino)
camino(Inicio, Fin, T, Camino) :-
  posLibre(Inicio,T),
  caminoAux(Inicio, Fin, T, [Inicio], CaminoDsdFin),
  reverse(CaminoDsdFin, Camino).

%% caminoAux(+Actual, +Fin, +Tablero, +Visitados, -Camino)
caminoAux(Fin, Fin, _, Camino, Camino).
caminoAux(Actual, Fin, T, Visitados, Camino) :-
  vecinoLibre(Actual, T, Vecino),
  not(member(Vecino, Visitados)),
  caminoAux(Vecino, Fin, T, [Vecino|Visitados], Camino).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

%% !!!!!!!!!!!! Usamos GyT, agregar comentario
camino2(Inicio, Fin, T, CaminoOrd) :-
  findall(Camino, camino(Inicio, Fin, T, Camino), Caminos),
  ordenarCaminos(Caminos, 1, CaminoOrd), not(CaminoOrd == []).

% ordenarCaminos(+ListaDeCaminos, +LongitudN, -CaminoDeLongitudN)
ordenarCaminos([], _, []) :- !.
ordenarCaminos(Caminos, LongActual, CaminoOrd) :-
  include(longitudEs(LongActual), Caminos, CaminosDeLongActual),
  (member(CaminoOrd, CaminosDeLongActual);
  subtract(Caminos, CaminosDeLongActual, NuevosCaminos),
  NuevaLong is LongActual + 1,
  ordenarCaminos(NuevosCaminos, NuevaLong, CaminoOrd)).

%% longitudEs(+Longitud, +Camino)
longitudEs(L, Camino) :- nonvar(L), nonvar(Camino), length(Camino, L).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

%% !!!!!!!!!!!! Usamos GyT, agregar comentario
%% caminoOptimo(+Inicio, +Fin, +Tablero, -CaminosOptimos)
caminoOptimo(Inicio, Fin, Tablero, CaminoOptimos) :-
  longCaminoOptimo(Inicio, Fin, Tablero, Min),
  findall(Camino, camino2(Inicio, Fin, Tablero, Camino), Caminos),
  include(longitudEs(Min), Caminos, CaminosOptimos),
  member(CaminoOptimos,CaminosOptimos).

longCaminoOptimo(Inicio, Fin, Tablero, Long) :-
  camino2(Inicio, Fin, Tablero, PrimerCaminoOptimo), !,
  nonvar(PrimerCaminoOptimo),
  length(PrimerCaminoOptimo, Long).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.

%% !!!!!!!!!!!! Usamos GyT, agregar comentario
caminoDual(Inicio, Fin, T1, T2, CaminoDual) :-
  findall(Camino, camino2(Inicio, Fin, T1, Camino), CaminosT1),
  findall(Camino, camino2(Inicio, Fin, T2, Camino), CaminosT2),
  intersection(CaminosT1, CaminosT2, CaminosDuales),
  member(CaminoDual, CaminosDuales).

%%%%%%%%%%%%%%%%%%%%%%
%% TABLEROS PARA TESTS
%%%%%%%%%%%%%%%%%%%%%%

tablero(ej2x2, T) :-
  tablero(2, 2, T),
  ocupar(pos(0, 1), T).

tablero(ej3x3, T) :-
  tablero(3,3,T),
  ocupar(pos(1, 2), T),
  ocupar(pos(0, 1), T),
  ocupar(pos(1, 1), T).

tablero(ej4x4, T) :-
  tablero(4,4,T),
  ocupar(pos(0, 0), T),
  ocupar(pos(0, 1), T),
  ocupar(pos(0, 3), T).

tablero(ej5x5, T) :-
  tablero(5, 5, T),
  ocupar(pos(1, 1), T),
  ocupar(pos(1, 2), T).

tablero(figura5izq, T) :-
  tablero(5,5,T),
  ocupar(pos(1,1),T),
  ocupar(pos(1,2),T).

tablero(figura5der, T) :-
  tablero(5,5,T),
  ocupar(pos(0,2),T),
  ocupar(pos(2,1),T),
  ocupar(pos(3,1),T).

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(5).
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(3) :- tablero(3, 2, T).
testTablero(4) :- tablero(3, 2, T), ocupar(pos(1, 0), T).
testTablero(5) :- not((tablero(ej2x2, T), ocupar(pos(3,3),T))).

cantidadTestsVecino(11).
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
testVecino(2) :- not(vecino(pos(0,0), [[_,_]], pos(0,0))).
testVecino(3) :- vecino(pos(2,2),[[_,_,_],[_,_,_],[_,_,_]], pos(2,1)).
testVecino(4) :- tablero(ej5x5, T), vecino(pos(0,0), T, V).
testVecino(5) :- tablero(ej5x5, T), vecino(pos(2,2), T, V).
testVecino(6) :- tablero(ej5x5, T), vecinoLibre(pos(0,0), T, V).
testVecino(7) :- not((tablero(ej5x5, T), vecinoLibre(pos(2,2), T, pos(2,2)))).
testVecino(8) :- not((tablero(ej5x5, T), vecinoLibre(pos(2,2), T, pos(10,10)))).
testVecino(9) :- not((tablero(ej5x5, T), vecinoLibre(pos(10,10), T, pos(2,2)))).
testVecino(10) :- not((tablero(ej5x5, T), vecinoLibre(pos(0,1), T, (1,1)))).
testVecino(11) :- not((tablero(ej5x5, T), vecinoLibre(pos(1,1), T, (0,1)))).

cantidadTestsCamino(14).
testCamino(1) :- tablero(ej5x5, T), camino(pos(0,0), pos(2,3), T, C).
testCamino(2) :- not((tablero(ej4x4, T), camino2(pos(0,0), pos(2,2), T, C))).
testCamino(3) :- tablero(ej4x4, T), camino2(pos(0,2), pos(2,2), T, C).
testCamino(4) :- tablero(3,3,T), ocupar(pos(1,1), T), camino(pos(0,0), pos(1,2), T, C).
testCamino(5) :- not((tablero(3,3,T), ocupar(pos(1,1), T), camino(pos(1,1), pos(1,2), T, C))).
testCamino(6) :- not((tablero(3,3,T), ocupar(pos(1,1), T), camino2(pos(1,1), pos(1,2), T, C))).
testCamino(7) :- not((tablero(3,3,T), ocupar(pos(1,1), T), camino2(pos(1,1), pos(4,4), T, C))).
testCamino(8) :- not((tablero(3,3,T), ocupar(pos(1,1), T), camino2(pos(4,4), pos(1,1), T, C))).
testCamino(9) :- not((tablero(3,3,T), ocupar(pos(1,1), T), camino2(pos(1,1), pos(1,1), T, C))).
testCamino(10) :- tablero(3,2,T), camino(pos(0,0), pos(2,1), T,C).  % 4 Caminos
testCamino(11) :- tablero(3,2,T), ocupar(pos(0,1),T), camino(pos(0,0), pos(2,1), T,C).  % 2 Caminos
testCamino(12) :- tablero(3,2,T), ocupar(pos(1,1),T), camino(pos(0,0), pos(2,1), T,C).  % 1 Camino
testCamino(13) :- not((tablero(3,2,T), ocupar(pos(0,0),T), camino(pos(0,0), pos(2,1), T,C))).  % 0 Caminos por Inicio Ocupado
testCamino(14) :- not((tablero(3,2,T), ocupar(pos(2,1),T), camino(pos(0,0), pos(2,1), T,C))).  % 0 Caminos por Fin Ocupado

cantidadTestsCaminoOptimo(3).
testCaminoOptimo(1) :- tablero(ej5x5, T), caminoOptimo(pos(0,0), pos(2,3), T, C), length(C, 6).
testCaminoOptimo(2) :- not((tablero(ej5x5, T), caminoOptimo(pos(0,0), pos(2,3), T, C), length(C, 5))).
testCaminoOptimo(3) :- not((tablero(ej5x5, T), caminoOptimo(pos(0,0), pos(2,3), T, C), length(C, 7))).

cantidadTestsCaminoDual(4).
% Caso 1. Hay dos caminos duales.
testCaminoDual(1) :- tablero(2, 2, T), tablero(5, 5, H), caminoDual(pos(0,0), pos(1,0), T, H, C).
% Caso 2. Hay un camino dual.
testCaminoDual(2) :- tablero(2, 2, T), tablero(5, 5, H), ocupar(pos(1,1), H), caminoDual(pos(0,0), pos(1,0), T, H, C).
% Caso 3. No hay ninguno.
testCaminoDual(3) :- not((tablero(2, 2, T), tablero(5, 5, H), ocupar(pos(1,0), H), caminoDual(pos(0,0), pos(1,0), T, H, C))).
% Caso 4. Figura 5.
testCaminoDual(4) :- tablero(figura5izq, Izq), tablero(figura5der, Der), caminoDual(pos(0,0), pos(4,3), Izq, Der, C).

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).
