% [LineNr, Expr, Rule]
% check if step belongs to prems
valid_line(Prems, _, [LineNr, Expr, Premise], _) :-
    member(Expr, Prems),
    write('line prems: '),
    write(LineNr), nl.

% check if line belongs to goal 
valid_line(_, Goal, [_, Expr, _], _) :-
    Expr = Goal.

% valid_proof calls on helper func w validated steps
valid_proof(Prems, Goal, ProofSteps) :-
    valid_proof_validator(Prems, Goal, ProofSteps, []).

% basfall
valid_proof_validator(_, Goal, [], ValidatedSoFar) :-
    write('hello'), nl,
    member([_,Goal,_], ValidatedSoFar), 
    write('basfall 2'), nl.


% helper func that stores checked lines in ValidatedSoFar
valid_proof_validator(Prems, Goal, [Step | RestOfProof], ValidatedSoFar) :-
    valid_line(Prems, Goal, Step, ValidatedSoFar), 
    valid_proof_validator(Prems, Goal, RestOfProof, [Step | ValidatedSoFar]).

% propositioner
imp(P, Q).    % p -> q
neg(P).      % !p
and(P, Q).   % p n q
or(P, Q).    % p v q

% Implication Introduction (→I)

% Implication Elimination (Modus Ponens, →E)
valid_line(_, _, [LineNr, ImpelExpr, impel(X, Y)], ValidatedSoFar) :-
    % write('1'),
    member([X, Antecedent, _], ValidatedSoFar),
    % write('2'),
    member([Y, imp(Antecedent, Conclusion), _], ValidatedSoFar),
    % write('3'),
    Conclusion = ImpelExpr,
    write('impel line: '),
    write(LineNr), nl.

% valid proof check recursive case

% Conjunction Introduction (∧I)

% Conjunction Elimination Left (∧E₁)

% Conjunction Elimination Right (∧E₂)

% Disjunction Introduction Left (∨I₁)

% Disjunction Introduction Right (∨I₂)

% Disjunction Elimination (∨E)

% Negation Introduction (¬I)

% Negation Elimination (¬E)

% Double Negation Elimination (¬¬E)

% Ex Falso (From contradiction, derive any formula)

% verify -> read file
verify(InputFileName) :- 
    see(InputFileName),     % open file with InputFileName
    read(Prems),            % read premises
    read(Goal),             % read goals
    read(Proof),            % read proof steps
    seen,                   % close file
    write(Prems), nl,
    write(Goal), nl,
    write(Proof), nl, nl,
    ( valid_proof(Prems, Goal, Proof) -> nl, write('yes'), nl ; nl, write('no') ), nl.
