/** types A Prolog implementation of a typed language

A Prolog implementation of a simple typed lambda 
calculus extended with records and subtyping rules. 
This languages has the following syntax:

 Term ::= Num
        | Bool 
        | Var 
        | Add Term Term
        | Sub Term Term 
        | And Term Term
        | Or Term Term
        | Not Term                   
        | Let Var Term Term 
        | IfThenElse Term Term Term              -- if then else 
        | \x -> Term                             -- abstraction 
        | Term Term                              -- application 
        | {x1: Term1, x2:Term2, ..., xn:Termn}   -- labeled record
        | Term.Label                             -- Projection

 Value ::= VInt 
         | VBool 
         | {x1: Value1, x:Value2, ..., xn:Valuen} 
         | \x -> Term 
There are two main predicates, eval and type_check. 
For example: 

==
1 ?- eval(add(num(3), num(4)), X). 
X = num(7) 
==
 
@author rbonifacio
*/


%% eval(Exp, Gamma, X) is det.
%
% An operational semantics of our simple typed lambda calculus language + extensions.
% Here we assume that, before evaluating the eval predicate, the expressions have been
% previously type checked.
%
% ==
% 1 ?- eval(proj(rec([item(x,num(3)), item(y, num(4))]),y), [], Res).
% Res = num(4)
% 2 ?- eval(let(inc, lambda(arg(x,int), add(var(x), num(1))), app(var(inc),num(5))), [], Res).
% Res = num(6) 
% == 
%


eval(num(N), _, num(N)).

eval(bool(B), _, bool(B)).

eval(lambda(arg(X, Type), Term), _, lambda(arg(X, Type), Term)).

eval(rec(ItemTerms), Gamma, rec(ItemValues)) :-
    eval_items(ItemTerms, Gamma, ItemValues). 

eval(proj(rec(Items), Label), Gamma, X) :-
    lookup_item(Label, Items, Term),
    eval(Term, Gamma, X).

eval(add(LHS, RHS), Gamma, num(X)) :-
    eval(LHS, Gamma, num(V1)),
    eval(RHS, Gamma, num(V2)),
    X is V1 + V2. 

eval(and(LHS, RHS), Gamma, bool(true)) :-
    eval(LHS, Gamma, bool(true)),
    eval(RHS, Gamma, bool(true)),
    !.                               % note the use of the cut operator here!

eval(and(_, _), _, bool(false)). 

eval(let(Var, Term1, Term2), Gamma, X) :-
    env(Var, Term1, Gamma, NGamma),
    eval(Term2, NGamma, X). 

eval(var(V), Gamma, X) :-
    lookup(V, Gamma, Term),
    eval(Term, Gamma, X). 
    
eval(app(Term1, Term2), Gamma, X) :-
    eval(Term1, Gamma, lambda(arg(Var, _), Body)),
    eval(Term2, Gamma, Argument),               % inner most strategy!
    env(Var, Argument, Gamma, NGamma),
    eval(Body, NGamma, X).

% other definitions of eval should be specified here!

% Next, we present the definition of some auxiliary predicates.

% First, a recursive definition of a predicate that evaluates
% all items of a record.

eval_items([], _, []). 
eval_items([item(L,T)|Tail], Gamma, ItemValues) :-
    eval(T, Gamma, V),
    eval_items(Tail, Gamma, Rec),
    ItemValues = [item(L,V)|Rec].

% Second, a predicate that introduces a new entry
% on an environment.

env(V, T, Gamma, [map(V, T)|Gamma]). 
    
%% The lookup predicates. 

lookup(Var, [map(Var, Term)|_], Term) :- !. 

lookup(Var, [map(_, _)|Gamma], Term) :- lookup(Var, Gamma, Term).

lookup_item(Label, [item(Label, Term)|_], Term) :- !.

lookup_item(Label, [item(_, _)|Tail], Term) :- lookup_item(Label, Tail, Term).

%% type_checker(Exp, Gamma, Type) is det.
%
% The definition of a type checker for our language. 
%
type_checker(top, _, top) :- !. 
type_checker(Term, Gamma, Type) :- tc(Term, Gamma, Type).
type_checker(Term, Gamma, Type) :-
    tc(Term, Gamma, S), 
    rec(S, Type).


tc(num(_),  _, int).

tc(bool(_), _, bool).

tc(lambda(arg(Arg, T1), Term), Gamma, arrow(T1, T2)) :-
    tc(Term, [map(Arg, T1)|Gamma], T2). 

tc(add(LHS, RHS), Gamma, int) :-
    tc(LHS, Gamma, int),
    tc(RHS, Gamma, int).

tc(and(LHS, RHS), Gamma, bool) :-
    tc(LHS, Gamma, bool),
    tc(RHS, Gamma, bool). 

tc(app(Term1, Term2), Gamma, T) :-
    tc(Term1, Gamma, arrow(T1, T)),
    tc(Term2, Gamma, T1).
    

:- begin_tests(tc).
test(tc_num)  :- findall(T, type_checker(num(5), [], T), Res), assertion(Res == [int, top]).
test(tc_bool) :- findall(T, type_checker(bool(true), [], T, Res), assertion(Res == [bool, int, top])). 
:- end_tests(tc).

subtype(bool, int).

subtype(rec(_), rec([])). 
subtype(rec(SubItems), rec([item(L, T)|SupItems])) :-
    lookup_item(L, SubItems, S),
    subtype(S, T),
    subtype(rec(SubItems), rec(SupItems)).

subtype(arr(S1, S2), arr(T1, T2)) :-
    subtype(T1, S1),
    subtype(S2, T2). 

rec(S, T)   :- subtype(S, T).
rec(_, top) :- !. 
rec(S, T)   :- subtype(S, U), rec(U, T). 

