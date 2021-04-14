%%%% -*- Mode: Prolog -*-

%%%% nfa.pl --
%%%%
%%%% Linguaggi di programmazione 2019-2020
%%%% Progetto Prolog gennaio 2020 E1P
%%%% Picozzi Riccardo - Mat: 816974
%%%% Paganini Andrea - Mat: 816028


%%% is_regexp/1
%%% controlla se RE è una espressione regolare
is_regexp(RE) :- symbol(RE), !.
is_regexp(RE) :- res_op(RE), !.

%%% symbol/1
%%% controlla se RE è un simbolo dell'alfabeto
symbol(RE) :-
    atomic(RE), !.
symbol(RE) :-
    compound_name_arguments(RE, F, _),
    F \= '[|]',
    F \= seq,
    F \= or,
    F \= plus,
    F \= star.
%%% res_op/1
%%% controlla se RE è una operazione riservata
res_op(RE) :- functor(RE, F, N),
    member(F, [seq, star, or, plus]),
    check_argument(RE, N).

check_argument(_, 0).
check_argument(RE, N) :-
    arg(N, RE, X),
    is_regexp(X),
    N1 is N - 1,
    check_argument(RE, N1).


%%% nfa_regexp_comp/2
%%% compilatore di espressioni regolari(RE) in
%%% NFA(identificato da FA_Id)
nfa_regexp_comp(FA_Id, RE) :-
    is_regexp(RE),
    assert(nfa_initial(FA_Id, initial)),
    assert(nfa_final(FA_Id, final)),
    nfa_regexp_comp(FA_Id, RE, initial, final).

%%% nfa_regexp_comp/4
%%% compila espressioni base (formate da un solo simbolo)
nfa_regexp_comp(FA_Id, RE, Init, Fin) :-
    symbol(RE),
    assert(nfa_delta(FA_Id, Init, RE, Fin)).
%%% compilatore per star(<RE>)
nfa_regexp_comp(FA_Id, RE, Init, Fin) :-
    RE =.. [star | Arg],
    assert(nfa_delta(FA_Id, Init, epsilon, Fin)),
    nodi_star(FA_Id, Arg, Fin, Fin).
%%% compilatore per or(<RE1>, <RE2>, ..., <REn>)
nfa_regexp_comp(FA_Id, RE, Init, Fin) :-
    RE =.. [or | Arg],
    nodi_or(FA_Id, Arg, Init, Fin).
%%% compilatore per seq(<RE1>, <RE2>, ..., <REn>)
nfa_regexp_comp(FA_Id, RE, Init, Fin) :-
    RE =.. [seq | Arg],
    nodi_seq(FA_Id, Arg, Init, Fin).
%%% compilatore per plus(<RE>)
nfa_regexp_comp(FA_Id, RE, Init, Fin) :-
    RE =.. [plus | Arg],
    nodi_plus(FA_Id, Arg, Init, Fin).

%%% nodi_star/4
%%% crea nodi e delta per le chiusure di Kleene
nodi_star(FA_Id, [X], Node, _) :-
    symbol(X),
    assert(nfa_delta(FA_Id, Node, X, Node)).
nodi_star(FA_Id, [X], Init, Fin) :-
    nfa_regexp_comp(FA_Id, X, Init, Fin).
%%% nodi_plus/4
%%% crea nodi e delta per le ripetizioni
nodi_plus(FA_Id, [X], Init, Fin) :-
    symbol(X),
    assert(nfa_delta(FA_Id, Init, X, Fin)),
    assert(nfa_delta(FA_Id, Fin, X, Fin)).
nodi_plus(FA_Id, [X], Init, Fin) :-
    nfa_regexp_comp(FA_Id, X, Init, Fin),
    assert(nfa_delta(FA_Id, Fin, epsilon, Init)).
%%% nodi_or/4
%%% crea nodi e delta per le alternative
nodi_or(_, [], _, _).
nodi_or(FA_Id, [X | Xs], Initial, Final) :-
    symbol(X),
    assert(nfa_delta(FA_Id, Initial, X, Final)),
    nodi_or(FA_Id, Xs, Initial, Final).
nodi_or(FA_Id, [X | Xs], Initial, Final) :-
    nfa_regexp_comp(FA_Id, X, Initial, Final),
    nodi_or(FA_Id, Xs, Initial, Final).
%%% nodi_seq/4
%%% crea nodi e delta per le sequenze
nodi_seq(FA_Id, [], Node, Final) :-
    assert(nfa_delta(FA_Id, Node, epsilon, Final)).
nodi_seq(FA_Id, [E], Node, Final) :-
    symbol(E),
    assert(nfa_delta(FA_Id, Node, E, Final)).
nodi_seq(FA_Id, [E | Es], Initial, Final) :-
    symbol(E),
    gensym(q, Node),
    assert(nfa_delta(FA_Id, Initial, E, Node)),
    nodi_seq(FA_Id, Es, Node, Final).
nodi_seq(FA_Id, [E | Es], Initial, Final) :-
    gensym(q, Node),
    nfa_regexp_comp(FA_Id, E, Initial, Node),
    nodi_seq(FA_Id, Es, Node, Final).


%%% nfa_test/2
%%% dato un input per un automa(FA_Id) controlla se
%%% l'input viene consumato completamente giungendo in
%%% uno stato d'accettazione/finale
nfa_test(FA_Id, Input) :- nfa_initial(FA_Id, S),
    accept(FA_Id, Input, S).

accept(FA_Id, [I | Is], S) :-
    nfa_delta(FA_Id, S, I, N), !,
    accept(FA_Id, Is, N).
accept(FA_Id, Is, S) :-
    nfa_delta(FA_Id, S, epsilon, N),
    accept(FA_Id, Is, N).
accept(FA_Id, [], Q) :- nfa_final(FA_Id, Q), !.
accept(FA_Id, [], Q) :-
    nfa_delta(FA_Id, Q, epsilon, N),
    nfa_final(FA_Id, N).


%%% nfa_clear/1
%%% rimuove dalla base di dati l'automa
%%% identificato da FA_Id
nfa_clear(FA_Id) :-
    retract(nfa_delta(FA_Id, _, _, _)),
    retract(nfa_initial(FA_Id, _)),
    retract(nfa_final(FA_Id, _)).
%%% nfa_clear/0
%%% rimuove dalla base di dati tutti gli automi definiti
nfa_clear :-
    retractall(nfa_initial(_, _)),
    retractall(nfa_final(_, _)),
    retractall(nfa_delta(_, _, _, _)).


%%% nfa_list/1
%%% lista stato iniziale, stato finale e
%%% delta dell'automa identificato da FA_Id
nfa_list(FA_Id) :-
    listing(nfa_initial(FA_Id, _)),
    listing(nfa_final(FA_Id, _)),
    listing(nfa_delta(FA_Id, _, _, _)).
%%% nfa_list/0
%%% lista stato iniziale, stato finale e
%%% delta di tutti gli automi definiti
nfa_list :-
    listing(nfa_initial(_, _)),
    listing(nfa_final(_, _)),
    listing(nfa_delta(_, _, _, _)).


%%%% end of file -- nfa.pl --

