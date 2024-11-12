% propositioner
imp(P, Q)   % p -> q
neg(P)      % !p
and(P, Q)   % p n q
or(P, Q)    % p v q

% regelappliceringar
% and introduction -> om X och Y sant så XnY sant
andint(X, Y) :-
    call(X),
    call(Y).

% and elimination 1
andel1(X) :-
    call(and(X, Y)).

% and elimination 2
andel2(X) :-
    call(and(Y, X)).

% or introduction 1
orint(X, Y) :-
    call(X).

% or introduction 2
orint(X, Y) :-
    call(Y).

% or elimination
orel()

% implication introduction
impint()

% implication elimination
impel(Y) :-
    call(imp(X, Y)),
    call(X).

% negation introduction
negint()

% negation elimination
negel()

% falsum rule
contel()

% double negation elimination -> !!p = p
negnegel()

% double negation introduction -> p = !!p
negnegint()

% MT rule -> p->q, !q => !p
mt()

% PBC rule -> !p ... cont => p
pbc()

% LEM rule -> p v !p
lem()


% valid proof check
valid_proof(Prems, Goal, Proof) :-
    % TODO : use apply_rule

% apply_rule predicate for each rule made

% verify -> read file
verify(InputFileName) :- 
    see(InputFileName),     % open file with InputFileName
    read(Prems),            % read premises
    read(Goal),             % read goals
    read(Proof),            % read proof steps
    seen,                   % close file
    valid_proof(Prems, Goal, Proof).    % check if proof is valid

