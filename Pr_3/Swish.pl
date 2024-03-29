﻿igual(X,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 1
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
duplica([],[]).
duplica([E|L],[E|[E|L1]]) :- duplica(L,L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 2
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
concatena([],L,L).
concatena([X|L],L1,[X|L2]) :- concatena(L,L1,L2).

invierte([],[]) :- !.
invierte([F|R],L) :- concatena(I, [F], L), invierte(R, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
palindromo(X) :- invierte(Y, X), igual(X,Y), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 4
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
divide(L,0,[],L) :- !.
divide([A|L],N,[A|L1],L2) :- N>0,succ(New, N),divide(L,New,L1,L2).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 5
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aplasta([], []) :- !.
aplasta([X], [X]) :- not(is_list(X)), !.
aplasta([X], Y) :- is_list(X), aplasta(X,Y), !.
aplasta([X|L], R) :- aplasta([X], LX), aplasta(L, LL), concatena(LX, LL, R).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 6
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
primos_rec(1,[],_).
primos_rec(N,[Pr|F],Pr) :- 0 is mod(N,Pr), New is N/Pr,
	primos_rec(New,F,Pr).
primos_rec(N,F,Pr) :- NewP is Pr+1, primos_rec(N,F,NewP).
primos(N,L) :- N>0, primos_rec(N,L,2), !.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 7
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Apartado 1

% Caso base cuando la lista es vac�a
cod_primero(X, [], [], [X]).

% Caso base cuando la lista tiene solo el elemento en cuest�n
cod_primero(X, [X], [], [X,X]).

% Caso base, cuando la lista empieza por un elemento diferente
cod_primero(X, [Y | Lrem], [Y | Lrem], [X]) :-
    not(igual(X, Y)).

% Funci�n general, recursiva. De una lista, calcula la lista de
% todos los elementos iniciales iguales a un elemento en cuesti�n
cod_primero(X, [X | RestoLista], Lrem, [X | Lfront]) :-
    cod_primero(X, RestoLista, Lrem, Lfront).


% Apartado 2

% Caso base de la funci�n cuando la lista es vac�a
cod_all([], []).

% Funci�n general que de una lista de elementos,
% forma listas de cada elemento
% Por ejemplo, de la lista
% [1, 2, 2, 3, 4, 4]
% devuelve la lista
% [[1], [2, 2], [3], [4, 4]]
cod_all([X | Rest], [Lfront | Lsiguiente]) :-
    cod_primero(X, Rest, Lrest, Lfront),
    cod_all(Lrest, Lsiguiente),
    !.


% Apartado 3

% Funci�n interfaz del ejercicio.
% De una lista de elementos, forma primero sus ternas correspondientes
% con cod_all, y despu�s genera la lista de pares correspondientes
run_length(L, Ret) :-
    cod_all(L, Ternas),
    cuenta_ternas(Ternas, Ret),
    !.

% Funi�n auxiliar que de una lista de ternas
% [[a, a, ..., a], ..., [z, z, ..., z]]
% devuelve la lista formada por sus pares
% [[na a], ..., [nz, z]]
% donde cada par representa el n�mero de veces que aparece el elemento
% y el elemento
cuenta_ternas([T], [L]) :- cuenta_terna(T, L), !.
cuenta_ternas([Terna|RTernas], [Par|RPares]) :-
    cuenta_terna(Terna, Par),
    cuenta_ternas(RTernas, RPares).

% Funci�n auxiliar que de una terna:
% [a, a, ..., a]
% devuelve el par correspondiente:
% [n, a]
% donde a es el elemento y n el n�mero de veces que aparece
cuenta_terna([X], [1, X]) :- !.
cuenta_terna([X | L], [Nnew, X]) :-
    cuenta_terna(L, [N,_]),
    Nnew is N+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Caso base: si hay un elemento, las dos hojas son  nil
build_tree([ER-_], Tree) :- igual(Tree,tree(ER, nil, nil)).
% Caso general: el primer elemento conforma la rama izquierda y el
% resto la derecha
build_tree([ER-PR|L],Tree) :-
	build_tree(L, NewR), build_tree([ER-PR],NewL),
    igual(Tree, tree(1,NewL,NewR)).

%%%%%%%%%%%%%%%%%%8.1%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funciones auxiliares para encontrar la info o una hoja del arbol
info(tree(I,_,_),I).
left(tree(_,L,_),L).
right(tree(_,_,R),R).

% Casos base: una de las hojas contiene el nodo que buscabamos
encode_elem(X1,X2,Tree):-
    left(Tree, L), info(L, IL), igual(IL,X1), igual(X2,[0]).
encode_elem(X1,X2,Tree):-
    right(Tree, R), info(R, IR), igual(IR,X1), igual(X2,[1]).
% Caso general: se a�ade un 1 a la codificacion y se concatena con
% el resultado que de la busqueda en la hoja derecha
encode_elem(X1,X2,Tree):-
    right(Tree,RTree),encode_elem(X1,New,RTree),concatena([1],New,X2).

% Caso especial: si la lista contiene un elemento
encode_elem(X1,[],Tree):- info(Tree, X1).

%%%%%%%%%%%%%%%%%%8.2%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Caso base: no hay elementos que codificar
encode_list([], [], _).
% Caso general: se codifica el primer elemento y se concatena
% recursivamente con el resto
encode_list([L|R],L2,Tree):-
    encode_elem(L,NewE,Tree), encode_list(R,NewL,Tree),
    concatena([NewE],NewL,L2).

%%%%%%%%%%%%%%%%%%8.3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,
             t,u,v,w,x,y,z]).

in_dict(A) :- dictionary(D), member(A,D).

% Algoritmo de ordenacion Quicksort
pivote(_, [], [], []).
pivote(Piv, [F|L], [F|Menor], Mayor) :-
    igual(F, _-P), Piv =< P, pivote(Piv, L, Menor, Mayor).
pivote(Piv, [F|L], Menor, [F|Mayor]) :- pivote(Piv, L, Menor, Mayor).

quicksort([], []).
quicksort([F|L], Ordenado) :- igual(F, _-P), pivote(P, L, L1, L2),
    quicksort(L1, Orden1), quicksort(L2, Orden2),
    concatena(Orden1, [F|Orden2], Ordenado), !.

% Se insertan los elementos de uno en un en la lista dada
insert_elem([E-P|L],E,New) :- NewP is P+1, igual(New,[E-NewP|L]).
insert_elem([E-P|L],Elem,New) :- not(igual(E,Elem)),
    insert_elem(L,Elem,NewL), igual(New,[E-P|NewL]).

%Construye una lista a partir del diccionario con todas las letras
%a peso 0
build_list_dict([],[]).
build_list_dict([E|D],[E-0|L]) :- build_list_dict(D,L).

build_list_rec([],L,NewL) :- igual(L,NewL).
build_list_rec([E|Elems],L,NewL) :- in_dict(E),
    insert_elem(L,E,Added), build_list_rec(Elems,Added,NewL).
build_list(Frase, Order) :- dictionary(D),build_list_dict(D,Init),
    build_list_rec(Frase,Init,L), quicksort(L,Order), !.

encode(L1,L2) :- build_list(L1, List), build_tree(List, Tree),
    encode_list(L1, L2, Tree).
