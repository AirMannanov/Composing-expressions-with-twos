:- module(twos, [
    target_expression/2
]).

% --- Подсчёт двоек в выражении ---
expr_twos(lit, 1).
expr_twos(add(A, B), Count) :-
    expr_twos(A, CA), expr_twos(B, CB),
    Count is CA + CB.
expr_twos(sub(A, B), Count) :-
    expr_twos(A, CA), expr_twos(B, CB),
    Count is CA + CB.
expr_twos(mul(A, B), Count) :-
    expr_twos(A, CA), expr_twos(B, CB),
    Count is CA + CB.
expr_twos(div(A, B), Count) :-
    expr_twos(A, CA), expr_twos(B, CB),
    Count is CA + CB.
expr_twos(sq(X), Count) :-
    expr_twos(X, Cx),
    Count is Cx + 1.

% --- Вычисление значения ---
expr_value(lit, 2).
expr_value(add(A, B), V) :-
    expr_value(A, VA), expr_value(B, VB),
    V is VA + VB.
expr_value(sub(A, B), V) :-
    expr_value(A, VA), expr_value(B, VB),
    V is VA - VB.
expr_value(mul(A, B), V) :-
    expr_value(A, VA), expr_value(B, VB),
    V is VA * VB.
expr_value(div(A, B), V) :-
    expr_value(A, VA), expr_value(B, VB),
    VB =\= 0,
    V is VA rdiv VB.
expr_value(sq(X), V) :-
    expr_value(X, VX),
    V is VX * VX.

% --- Генерация выражений ---

%% expression_of_twos(Count, Expr, Value) is nondet.
%  Строит выражение из Count двоек и возвращает его значение.
expression_of_twos(1, lit, 2).
expression_of_twos(Count, Expr, Value) :-
    Count > 1,
    Count1 is Count - 1,
    between(1, Count1, LeftCount),
    RightCount is Count - LeftCount,
    expression_of_twos(LeftCount, LeftExpr, LeftValue),
    expression_of_twos(RightCount, RightExpr, RightValue),
    combine_expr(LeftExpr, LeftValue, RightExpr, RightValue, Expr, Value).
expression_of_twos(Count, sq(SubExpr), Value) :-
    Count > 1,
    Count1 is Count - 1,
    expression_of_twos(Count1, SubExpr, SubValue),
    Value is SubValue * SubValue.

combine_expr(LE, LV, RE, RV, add(LE, RE), V) :-
    canonical_pair(LE, RE),
    V is LV + RV.
combine_expr(LE, LV, RE, RV, mul(LE, RE), V) :-
    canonical_pair(LE, RE),
    V is LV * RV.
combine_expr(LE, LV, RE, RV, sub(LE, RE), V) :-
    V is LV - RV.
combine_expr(LE, LV, RE, RV, div(LE, RE), V) :-
    RV =\= 0,
    V is LV rdiv RV.

% Канонический порядок коммутативных пар, чтобы не дублировать перестановки
% разрешаем только те пары (A, B), где A меньше либо равен B по порядку термов
canonical_pair(A, B) :- ( A @< B ; A == B ).

% --- Поиск выражения с заданным целым значением ---

%% target_expression(+N, -Expr) is nondet.
%  Находит выражение Expr из пяти двоек, значение которого равно целому N.
target_expression(N, Expr) :-
    expression_of_twos(5, Expr, Value),
    integer_fraction(Value, N),
    expr_twos(Expr, 5).

integer_fraction(Value, N) :-
    integer(Value),
    N is Value.

% --- Преобразование выражений в строку ---

expr_to_string(lit, '2', 0).
expr_to_string(add(A,B), S, 1) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    ( PA > 1 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB > 1 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w + ~w', [SA1, SB1]).
expr_to_string(sub(A,B), S, 1) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    ( PA > 1 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB >= 1 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w - ~w', [SA1, SB1]).
expr_to_string(mul(A,B), S, 2) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    ( PA > 2 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB > 2 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w * ~w', [SA1, SB1]).
expr_to_string(div(A,B), S, 2) :-
    expr_to_string(A, SA, PA),
    expr_to_string(B, SB, PB),
    ( PA > 2 -> format(atom(SA1), '(~w)', [SA]) ; SA1 = SA ),
    ( PB >= 2 -> format(atom(SB1), '(~w)', [SB]) ; SB1 = SB ),
    format(atom(S), '~w : ~w', [SA1, SB1]).
expr_to_string(sq(X), S, 3) :-
    expr_to_string(X, SX, PX),
    ( PX >= 3 -> format(atom(SX1), '(~w)', [SX]) ; SX1 = SX ),
    format(atom(S), '~w ^ 2', [SX1]).

main(N, StringExpr) :- 
    target_expression(N, Expr),
    expr_to_string(Expr, StringExpr, _).
