/*
Intution of program:-
    -> initialization calles the starter function
    -> Then it reads two integers
    -> Checker checks its validity
    -> queen is called 
            queen uses permutation of 7 numbers 1 to 8
            execept the initial specified position.
            Then insertno inserts the input at proper position.
            combine and diagonal function checks the validty of that permutation.
    
    in queen function P is the list of 8 integers 1 to 8 
    specifying the position of queen eg L[1] = 4 means queen 
    is at (4, 1)

    logic -> adding and subtracting the permutation with the row list 
                gives the direction of vector of that queen. So, if two 
                queen have same direction than the permuation is not valid.

    So, the queen function uses permutation of 7 numbers unlike taking permutaion
    of 8 numbers and checking validity of specified input.(Decrease of 15000 iterations)
*/

:- initialization(starter).

starter :-
    read_number(A),
    read_number(B), 
    checker(A, B, Condition),
    Condition =:= 1,
    queen(P, A, B);
    write('IMPOSSIBLE\n').

% function which checks the validity of input variables.
checker(A, B, Condition) :- A > 0, A < 9, B > 0, B < 9, Condition is 1.
checker(A, B, 0).


% Main queen function

queen(P, A, B) :- 
    deleteInitial(A,[1,2,3,4,5,6,7,8],L),
    !,  					                %this cut is for increasing efficency
    permutation(L, Z),  				    % taking permutaion of list of 7 numbers
    insertno(Z, A, B - 1, P),   		    % inserting the initial position
    calculate([1,2,3,4,5,6,7,8], P, S, D), 	% making sum and diffrence list
    diagonal(S),
    diagonal(D), 				            % checking no two members in list are same
    printTable(P), 				            % function for printing the table
    !;
    write('IMPOSSIBLE\n').                  % cut to stop finding new solutions after finding one.


% function calculates the sum and diffrence lists
calculate([X1|X], [Y1|Y], [S1|S], [D1|D]) :-
    S1 is X1 + Y1,
    D1 is X1 - Y1,
    calculate(X, Y, S, D).
calculate([], [], [], []).

% it checks that all the element in the list are diffrent
diagonal([X|Y]) :- \+member(X, Y), diagonal(Y).
diagonal([X]).

% this function inserts a number at a specified postion in list.
insertno(X, Y, P, Z) :- P =:= 7, insertAtEnd(Y,X,Z), !.
insertno([X|T], Y, P, [X|L]) :- P > 0, insertno(T, Y, P - 1, L), !. % this cut is for efficency and protecting it from wrong solutions.
insertno([X|T], Y, P, [Y|[X|T]]).

insertAtEnd(X, Y, Z) :- append(Y, [X], Z).

%function for deleting the initial postion of queen from start list 
deleteInitial(E, [E|T], T).
deleteInitial(E, [X|T], [X|T1]) :- deleteInitial(E, T, T1).
                
%functions for printing a row of  table 
printRow(A, B) :- B =:= 9, write('\n'), !. 
printRow(A, B) :- A =:= B,
                  write('1 '),
                  printRow(A, B+1);
                  A =\= B, 
                  write('0 '), 
                  printRow(A, B+1).

%funciton for printing the entire table.
printTable([X|T]) :- printRow(X, 1), printTable(T).
printTable([]).





