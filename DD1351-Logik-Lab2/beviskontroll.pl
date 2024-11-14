% [LineNr, Expr, Rule]
% check if step belongs to prems
valid_line(Prems, _, [LineNr, Expr, Premise], _) :-
    member(Expr, Prems),
    write('line prems: '),
    write(LineNr),
    write('\n'). 

% check if line belongs to goal 
valid_line(_, Goal, [LineNr, Expr, _], _) :-
    member(Expr, Goal),
    write('linenr goal: '),
    write(LineNr),
    write('\n').

% valid_proof calls on helper func w validated steps
valid_proof(Prems, Goal, ProofSteps) :-
    valid_proof_validator(Prems, Goal, ProofSteps, []).

% basfall
valid_proof_validator(_, Goal, [], ValidatedSoFar) :-
    write(Goal),
    write(ValidatedSoFar),
    member(Goal, ValidatedSoFar).

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
    write('1'),
    nth1(X, ValidatedSoFar, [X, Antecedent, _]),
    write('2'),
    nth1(Y, ValidatedSoFar, [Y, imp(Antecedent, Conclusion), _]),
    write('3'),
    Conclusion = ImpelExpr,
    write('impen line: '),
    write(LineNr),
    write('\n').

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
    write(Prems),
    write('\n'),
    write(Goal),
    write('\n'),
    write(Proof),
    write('\n'),
    ( valid_proof(Prems, Goal, Proof) -> write('yes') ; write('no') ).
