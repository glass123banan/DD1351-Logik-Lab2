% propositions: implication, negation, and, or
imp(P, Q).   % p -> q
neg(P).      % !p
and(P, Q).   % p n q
or(P, Q).    % p v q

% valid_proof calls on helper func w validated steps
valid_proof(Prems, Goal, ProofSteps) :-
    valid_proof_validator(Prems, Goal, ProofSteps, []).

% basecase: check if Goal is present in ValidatedSoFar
valid_proof_validator(_, Goal, [], [LastStep | ValidatedSoFar]) :-
    \+ [_, Goal, assumption] = LastStep,
    [_,Goal,_] = LastStep.

% box recursive case: checks if its a box -> handles boxes
valid_proof_validator(Prems, Goal, [Box | RestOfProof], ValidatedSoFar) :-
    is_box(Box, ValidatedSoFar),       
    check_boxes(Box, ValidatedSoFar),  % Validate the content of the box
    !,      % if find box, dont go to recursive case
    valid_proof_validator(Prems, Goal, RestOfProof, [Box | ValidatedSoFar]).  

% recursive case: helper func that stores checked lines in ValidatedSoFar -> handles regular lines
valid_proof_validator(Prems, Goal, [Step | RestOfProof], ValidatedSoFar) :-
    \+is_box(Step, ValidatedSoFar),
    valid_line(Prems, Goal, Step, ValidatedSoFar), 
    valid_proof_validator(Prems, Goal, RestOfProof, [Step | ValidatedSoFar]).

% box control - If list in list and first step is assumption -> box
is_box(BoxStep, ValidatedSoFar) :-
    is_list(BoxStep),
    BoxStep = [[_,_, assumption]| _].

% recursively checks steps inside a box
check_boxes([], _).

check_boxes([[LineNr, Expr, assumption] | RestOfBox], ValidatedSoFar) :-
    \+ member([_, _, assumption], RestOfBox),
    check_boxes(RestOfBox, [[LineNr, Expr, assumption] | ValidatedSoFar]).

% If the step is a nested box, validate it recursively
check_boxes([NestedBox | RestOfBox], ValidatedSoFar) :-
    is_box(NestedBox, ValidatedSoFar),                 
    check_boxes(NestedBox, ValidatedSoFar),  % Validate the nested box                  
    check_boxes(RestOfBox, [NestedBox |ValidatedSoFar]). 

check_boxes([StepInBox|RestOfBox], ValidatedSoFar) :-
    \+is_box(StepInBox, ValidatedSoFar),
    \+ StepInBox = [_,_, assumption],
    valid_line(_,_,StepInBox, ValidatedSoFar),
    check_boxes(RestOfBox, [StepInBox | ValidatedSoFar]). 

% Check each individual line if its valid
% Format per proof line: [LineNr, Expr, Rule]

% check if step belongs to prems
valid_line(Prems, _, [LineNr, Expr, premise], _) :-
    member(Expr, Prems),
    !.

valid_line(_, _, [LineNr, Expr, assumption], _) :-
    is_box([LineNr, Expr, assumption], ValidatedSoFar).     % problem line

% copy rule
valid_line(_,_, [LineNr, CopyExpr, copy(X)], ValidatedSoFar) :-
    member([X, CopiedExpr, _], ValidatedSoFar),
    CopyExpr = CopiedExpr.

% Last predicate to find last element in list 
% Basecase: If list has only X, X is last element
last(X,[X]).

% Recursive case: ignore head and match X with Z
last(X,[_|Z]) :- 
    last(X,Z).

% Implication Introduction (→I) -> need box
% [Box | _] from ValidatedSoFar (in valid_proof_validator)
valid_line(_,_, [LineNr, imp(Antecedent, Conclusion), impint(X,Y)], ValidatedSoFar) :-
    find_box(ValidatedSoFar, X, Y, Box),
    member([X, Antecedent, assumption], Box),
    last([Y, Conclusion, _], Box).

% Implication Elimination (Modus Ponens, →E)
valid_line(_, _, [LineNr, ImpelExpr, impel(X, Y)], ValidatedSoFar) :-
    member([X, Antecedent, _], ValidatedSoFar),                         % P is true
    member([Y, imp(Antecedent, Conclusion), _], ValidatedSoFar),        % P->Q is true
    Conclusion = ImpelExpr.

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
valid_line(_,_,[LineNr, OrExpr, orint2(X)], ValidatedSoFar) :-
    member([X, Disjunct2, _], ValidatedSoFar),
    OrExpr = or(Disjunct1, Disjunct2).

% Helper predicate to find a box (subproof) by start and end line numbers
find_box(ValidatedSoFar, StartLine, EndLine, Box) :-
    member(Box, ValidatedSoFar),
    Box = [[StartLine, _, assumption] |_],
    member([EndLine, _, _], Box).

% Disjunction Elimination (∨E)
valid_line(_, _, [LineNr, OrelExpr, orel(OrExprLine, StartFirst, EndFirst, StartSecond, EndSecond)], ValidatedSoFar) :-
    OrExpr = or(LeftDisjunct, RightDisjunct),
    member([OrExprLine, OrExpr, _], ValidatedSoFar),
    find_box(ValidatedSoFar, StartFirst, EndFirst, Box1), 
    Box1 = [[StartFirst, LeftDisjunct, assumption] | _],
    last([EndFirst, OrelExpr, _], Box1),
    find_box(ValidatedSoFar, StartSecond, EndSecond, Box2),
    Box2 = [[StartSecond, RightDisjunct, assumption] | _],
    last([EndSecond, OrelExpr, _], Box2).

% Negation Introduction (¬I) -> need box
valid_line(_,_,[LineNr, neg(Negand), negint(X, Y)], ValidatedSoFar) :-
    find_box(ValidatedSoFar, X, Y, Box), 
    member([X, Negand, assumption], Box),
    last([Y, cont, _], Box).

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
valid_line(_,_, [LineNr, Expr, pbc(X, Y)], ValidatedSoFar) :-
    find_box(ValidatedSoFar, X, Y, Box), 
    member([X, neg(Expr), assumption], Box), 
    last([Y, cont, _], Box).

% Law of the Excluded Middle (LEM)
valid_line(_,_,[LineNr, or(LemExpr, neg(LemExpr)), lem], ValidatedSoFar).

% verify -> read file
verify(InputFileName) :- 
    see(InputFileName),     % open file with InputFileName
    read(Prems), read(Goal), read(Proof),            
    seen,
    valid_proof(Prems, Goal, Proof).
