% propositioner
imp(P, Q).    % p -> q
neg(P).      % !p
and(P, Q).   % p n q
or(P, Q).    % p v q

% check if step belongs to prems
valid_line(Prems, _, Step) :-
    member(Step, Prems). 

% Implication Introduction (→I)
valid_line(Prems, imp(P, Q), impint(Assumptions, Conclusion)) :-
    append(Assumptions, [P], ExtendedPrems),
    valid_proof(ExtendedPrems, Q, Conclusion).

% Implication Elimination (Modus Ponens, →E)
valid_line(Prems, Goal, impel(Implication, Premise)) :-
    valid_line(Prems, imp(P, Goal), Implication),
    valid_line(Prems, P, Premise).

% Conjunction Introduction (∧I)
valid_line(Prems, and(P, Q), andint(Proof1, Proof2)) :-
    valid_line(Prems, P, Proof1),
    valid_line(Prems, Q, Proof2).

% Conjunction Elimination Left (∧E₁)
valid_line(Prems, P, andel1(Conjunction)) :-
    valid_line(Prems, and(P, _), Conjunction).

% Conjunction Elimination Right (∧E₂)
valid_line(Prems, Q, andel2(Conjunction)) :-
    valid_line(Prems, and(_, Q), Conjunction).

% Disjunction Introduction Left (∨I₁)
valid_line(Prems, or(P, _), orint1(Proof)) :-
    valid_line(Prems, P, Proof).

% Disjunction Introduction Right (∨I₂)
valid_line(Prems, or(_, Q), orint2(Proof)) :-
    valid_line(Prems, Q, Proof).

% Disjunction Elimination (∨E)
valid_line(Prems, Goal, orel(Disjunction, Proof1, Proof2)) :-
    valid_line(Prems, or(P, Q), Disjunction),
    valid_proof([P | Prems], Goal, Proof1),
    valid_proof([Q | Prems], Goal, Proof2).

% Negation Introduction (¬I)
valid_line(Prems, neg(P), negint(Assumption, Proof)) :-
    append(Prems, [P], ExtendedPrems),
    valid_proof(ExtendedPrems, contradiction, Proof).

% Negation Elimination (¬E)
valid_line(Prems, contradiction, negel(Proof)) :-
    valid_line(Prems, neg(P), Proof).

% Double Negation Elimination (¬¬E)
valid_line(Prems, P, negnegel(Proof)) :-
    valid_line(Prems, neg(neg(P)), Proof).

% Ex Falso (From contradiction, derive any formula)
valid_line(Prems, _, contel(Proof)) :-
    valid_line(Prems, contradiction, Proof).

% valid proof check
valid_proof(_, _, []).
valid_proof(Prems, Goal, [Step | RestOfProof]) :-
    valid_line(Prems, Goal, Step),
    valid_proof(Prems, Goal, RestOfProof).

% verify -> read file
verify(InputFileName) :- 
    see(InputFileName),     % open file with InputFileName
    read(Prems),            % read premises
    read(Goal),             % read goals
    read(Proof),            % read proof steps
    seen,                   % close file
    ( valid_proof(Prems, Goal, ProofSteps) -> write('yes') ; write('no') ).
