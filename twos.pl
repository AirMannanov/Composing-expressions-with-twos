:- module(twos, [
    target_expression/2
]).

% --- Предикат для чисел из K двоек: 2, 22, 222, 2222 ---
twos_value(1, 2).
twos_value(K, V) :-
    K > 1, K1 is K - 1,
    twos_value(K1, V1),
    V is V1 * 10 + 2.

% --- Подсчёт двоек в выражении ---
expr_twos(lit(K), K) :- between(1,4, K).
expr_twos(add(A, B), C) :- expr_twos(A, CA), expr_twos(B, CB), C is CA + CB.
expr_twos(sub(A, B), C) :- expr_twos(A, CA), expr_twos(B, CB), C is CA + CB.
expr_twos(mul(A, B), C) :- expr_twos(A, CA), expr_twos(B, CB), C is CA + CB.
expr_twos(div(A, B), C) :- expr_twos(A, CA), expr_twos(B, CB), C is CA + CB.
expr_twos(sq(X), C)     :- expr_twos(X, CX), C is CX + 1.

% --- Вычисление значения ---
expr_value(lit(K), V)  :- twos_value(K, V).
expr_value(add(A, B), V):- expr_value(A, VA), expr_value(B, VB), V is VA + VB.
expr_value(sub(A, B), V):- expr_value(A, VA), expr_value(B, VB), V is VA - VB.
expr_value(mul(A, B), V):- expr_value(A, VA), expr_value(B, VB), V is VA * VB.
expr_value(div(A, B), V):- expr_value(A, VA), expr_value(B, VB), VB =\= 0, V is VA rdiv VB.
expr_value(sq(X), V)   :- expr_value(X, VX), V is VX*VX.

% --- Генерация выражений ---
expression_of_twos(K, lit(K), V) :-
    between(1, 4, K),
    twos_value(K, V).
expression_of_twos(Count, Expr, Value) :-
    Count > 1,
    Count1 is Count - 1,
    between(1, Count1, LeftCount),
    RightCount is Count - LeftCount,
    expression_of_twos(LeftCount, LeftExpr, LeftValue),
    expression_of_twos(RightCount, RightExpr, RightValue),
    combine_expr(LeftExpr, LeftValue, RightExpr, RightValue, Expr, Value).
expression_of_twos(Count, sq(SubExpr), Value) :-
    Count > 1, Count1 is Count - 1,
    expression_of_twos(Count1, SubExpr, SubValue),
    Value is SubValue * SubValue.

combine_expr(LE, LV, RE, RV, add(LE, RE), V) :- canonical_pair(LE, RE), V is LV + RV.
combine_expr(LE, LV, RE, RV, mul(LE, RE), V) :- canonical_pair(LE, RE), V is LV * RV.
combine_expr(LE, LV, RE, RV, sub(LE, RE), V) :- V is LV - RV.
combine_expr(LE, LV, RE, RV, div(LE, RE), V) :- RV =\= 0, V is LV rdiv RV.

% Канонический порядок коммутативных пар, чтобы не дублировать перестановки
% разрешаем только те пары (A, B), где A меньше либо равен B по порядку термов
canonical_pair(A, B) :- ( A @< B ; A == B ).

% --- Поиск выражения с заданным целым значением ---
target_expression(N, Expr) :-
    expression_of_twos(5, Expr, V),
    integer(V),
    N is V.

% --- Преобразование выражений в строку (минимум скобок) ---
expr_to_string(E, S) :- expr_to_string(E, S, _).

expr_to_string(lit(K), S, 0) :-
    twos_value(K, V),
    format(atom(S), '~w', [V]).
expr_to_string(add(A,B), S, 1) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    % ( PA > 1 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    % ( PB > 1 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w + ~w', [SA, SB]).
expr_to_string(sub(A,B), S, 1) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    % ( PA > 1 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB =< 1, PB \= 0 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w - ~w', [SA, SB1]).
expr_to_string(mul(A,B), S, 2) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    ( PA < 2, PA \= 0 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB < 2, PB \= 0 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w * ~w', [SA1, SB1]).
expr_to_string(div(A,B), S, 2) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    ( PA < 2, PA \= 0 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB =< 2, PB \= 0 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w : ~w', [SA1, SB1]).
expr_to_string(sq(X), S, 3) :-
    expr_to_string(X, SX, PX),
    ( PX =< 3, PX \= 0 -> format(atom(SX1), '(~w)', [SX]) ; SX1 = SX ),
    format(atom(S), '~w ^ 2', [SX1]).

main(N, StringExpr) :- 
    target_expression(N, Expr),
    expr_to_string(Expr, StringExpr).
