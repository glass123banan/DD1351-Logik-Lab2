% propositions: implication, negation, and, or
imp(P, Q).   % p -> q
neg(P).      % !p
and(P, Q).   % p n q
or(P, Q).    % p v q

% valid_proof calls on helper func w validated steps
valid_proof(Prems, Goal, ProofSteps) :-
    valid_proof_validator(Prems, Goal, ProofSteps, []).

% basecase: check if Goal is present in ValidatedSoFar
valid_proof_validator(_, Goal, [], ValidatedSoFar) :-
    member([_,Goal,_], ValidatedSoFar).

% helper func that stores checked lines in ValidatedSoFar
valid_proof_validator(Prems, Goal, [Step | RestOfProof], ValidatedSoFar) :-
    valid_line(Prems, Goal, Step, ValidatedSoFar), 
    valid_proof_validator(Prems, Goal, RestOfProof, [Step | ValidatedSoFar]).

% Check each individual line if its valid
% Format per proof line: [LineNr, Expr, Rule]
% check if step belongs to prems
valid_line(Prems, _, [LineNr, Expr, Premise], _) :-
    member(Expr, Prems).

% Implication Introduction (→I) -> need box

% Implication Elimination (Modus Ponens, →E)
valid_line(_, _, [LineNr, ImpelExpr, impel(X, Y)], ValidatedSoFar) :-
    member([X, Antecedent, _], ValidatedSoFar),                         % P is true
    member([Y, imp(Antecedent, Conclusion), _], ValidatedSoFar),        % P->Q is true
    Conclusion = ImpelExpr.                                             % Q is thus true

% Conjunction Introduction (∧I)
valid_line(_, _, [LineNr, AndExpr, andint(X, Y)], ValidatedSoFar) :-
    member([X, Conjunct1, _], ValidatedSoFar),
    member([Y, Conjunct2, _], ValidatedSoFar), 
    AndExpr = and(Conjunct1, Conjunct2).

% Conjunction Elimination Left (∧E₁)
valid_line(_,_,[LineNr, AndExpr, andel1(X)], ValidatedSoFar) :-
    member([X, and(Conjunct1, Conjunct2), _], ValidatedSoFar),
    AndExpr = Conjunct1.

% Conjunction Elimination Right (∧E₂)
valid_line(_,_,[LineNr, AndExpr, andel2(X)], ValidatedSoFar) :-
    member([X, and(Conjunct1, Conjunct2), _], ValidatedSoFar),
    AndExpr = Conjunct2.

% Disjunction Introduction Left (∨I₁)
valid_line(_,_,[LineNr, OrExpr, orint1(X)], ValidatedSoFar) :-
    member([X, Disjunct1, _], ValidatedSoFar),
    OrExpr = or(Disjunct1, Disjunct2).

% Disjunction Introduction Right (∨I₂)
valid_line(_,_,[LineNr, OrExpr, orint1(X)], ValidatedSoFar) :-
    member([X, Disjunct2, _], ValidatedSoFar),
    OrExpr = or(Disjunct1, Disjunct2).

% Disjunction Elimination (∨E) -> need box

% Negation Introduction (¬I) -> need box

% Negation Elimination (¬E)
valid_line(_,_,[LineNr, NegExpr, negel(X, Y)], ValidatedSoFar) :-
    member([X, Negand, _], ValidatedSoFar),
    member([Y, neg(Negand), _], ValidatedSoFar),
    NegExpr = cont.

% Double Negation Elimination (¬¬E)
valid_line(_,_,[LineNr, DoubleNegExpr, negnegel(X)], ValidatedSoFar) :-
    member([X, neg(neg(Expr)), _], ValidatedSoFar),
    DoubleNegExpr = Expr.

% Double Negation Introduction
valid_line(_,_, [LineNr, Expr, negnegint(X)], ValidatedSoFar) :-
    member([X, Negand, _], ValidatedSoFar),
    Expr = neg(neg(Negand)).

% Contradiction Elimination (⊥e)
valid_line(_,_, [LineNr, Expr, contel(X)], ValidatedSoFar) :-
    member([X, cont, _], ValidatedSoFar).

% Modus Tollens (MT)
valid_line(_,_,[LineNr, MtExpr, mt(X, Y)], ValidatedSoFar) :-
    member([X, imp(Antecedent, Conclusion), _], ValidatedSoFar),
    member([Y, neg(Conclusion), _], ValidatedSoFar),
    MtExpr = neg(Antecedent).

% PBC rule -> need box

% Law of the Excluded Middle (LEM)
valid_line(_,_,[LineNr, or(LemExpr, neg(LemExpr)), lem], ValidatedSoFar).

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
