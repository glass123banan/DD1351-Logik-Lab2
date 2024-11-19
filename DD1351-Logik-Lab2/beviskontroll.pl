% valid_proof calls on helper func w validated steps
valid_proof(Prems, Goal, ProofSteps) :-
    valid_proof_validator(Prems, Goal, ProofSteps, []).

% basecase: check if Goal is present in ValidatedSoFar
valid_proof_validator(_, Goal, [], [LastStep | ValidatedSoFar]) :-
    \+ [_, Goal, assumption] = LastStep,
    [_,Goal,_] = LastStep.
    write("Proof valid since last step is: "), write(LastStep), write(" "), write(Goal), nl. 

% box recursive case: checks if its a box -> handles boxes
valid_proof_validator(Prems, Goal, [Box | RestOfProof], ValidatedSoFar) :-
    is_box(Box, ValidatedSoFar),       
    write("Box: "), write(Box), nl, 
    write("Rest of proof: "), write(RestOfProof), nl,
    check_boxes(Box, ValidatedSoFar),  % Validate the content of the box
    !,      % if find box, dont go to recursive case
    valid_proof_validator(Prems, Goal, RestOfProof, [Box | ValidatedSoFar]).  

% recursive case: helper func that stores checked lines in ValidatedSoFar -> handles regular lines
valid_proof_validator(Prems, Goal, [Step | RestOfProof], ValidatedSoFar) :-
    \+is_box(Step, ValidatedSoFar),
    write("normal line check: "), write(Step), nl,
    write("Validated so far (from normal): "), write(ValidatedSoFar), nl, 
    valid_line(Prems, Goal, Step, ValidatedSoFar), 
    valid_proof_validator(Prems, Goal, RestOfProof, [Step | ValidatedSoFar]).

% box control - If list in list and first step is assumption -> box
is_box(BoxStep, ValidatedSoFar) :-
    is_list(BoxStep),
    BoxStep = [[_,_, assumption]| _],
    write("Box structure identified: "), write(BoxStep), nl.
    % check_boxes(BoxStep, ValidatedSoFar).

% recursively checks steps inside a box
check_boxes([], _).

check_boxes([[LineNr, Expr, assumption] | RestOfBox], ValidatedSoFar) :-
    \+ member([_, _, assumption], RestOfBox),
    write("Box assumption validated: "), write([LineNr, Expr, assumption]), nl,
    check_boxes(RestOfBox, [[LineNr, Expr, assumption] | ValidatedSoFar]).

% If the step is a nested box, validate it recursively
check_boxes([NestedBox | RestOfBox], ValidatedSoFar) :-
    is_box(NestedBox, ValidatedSoFar),                 
    write("Checking nested box: "), write(NestedBox), nl, 
    check_boxes(NestedBox, ValidatedSoFar),  % Validate the nested box                  
    check_boxes(RestOfBox, [NestedBox |ValidatedSoFar]). 

check_boxes([StepInBox|RestOfBox], ValidatedSoFar) :-
    \+ is_box(StepInBox, ValidatedSoFar),
    \+ StepInBox = [_,_, assumption],
    write("Checking step in box: "), write(StepInBox), nl,
    write("validated box proof: "), write(ValidatedSoFar), nl, 
    valid_line(_,_,StepInBox, ValidatedSoFar),
    write("Step in box validated: "), write(StepInBox), nl,
    check_boxes(RestOfBox, [StepInBox | ValidatedSoFar]). 

% Check each individual line if its valid
% Format per proof line: [LineNr, Expr, Rule]

% check if step belongs to prems
valid_line(Prems, _, [LineNr, Expr, premise], _) :-
    member(Expr, Prems),
    !,
    write("is part of premise"), nl.

valid_line(_, _, [LineNr, _, assumption], _) :-
    is_box([LineNr, Expr, assumption], ValidatedSoFar),     % problem line?
    write("is assumption"), nl.

% copy rule
valid_line(_,_, [LineNr, CopyExpr, copy(X)], ValidatedSoFar) :-
    member([X, CopiedExpr, _], ValidatedSoFar),
    CopyExpr = CopiedExpr.
    write("copy rule"), nl.

last(X,[X]).
last(X,[_|Z]) :- 
    last(X,Z).

% Implication Introduction (→I) -> need box
% [Box | _] from ValidatedSoFar (in valid_proof_validator)
valid_line(_,_, [LineNr, imp(Antecedent, Conclusion), impint(X,Y)], ValidatedSoFar) :-
    % write("Antecedent: "), write(Antecedent), nl, 
    % write("Conclusion: "), write(Conclusion), nl, 
    find_box(ValidatedSoFar, X, Y, Box),
    member([X, Antecedent, assumption], Box),
    % member([Y, Conclusion, _], Box),
    last([Y, Conclusion, _], Box),
    write("Validated box: "), write(Box), nl, 
    write("impint"), nl.

% Implication Elimination (Modus Ponens, →E)
valid_line(_, _, [LineNr, ImpelExpr, impel(X, Y)], ValidatedSoFar) :-
    member([X, Antecedent, _], ValidatedSoFar),                         % P is true
    member([Y, imp(Antecedent, Conclusion), _], ValidatedSoFar),        % P->Q is true
    Conclusion = ImpelExpr,                                             % Q is thus true
    write("impel"), nl.

% Conjunction Introduction (∧I)
valid_line(_, _, [LineNr, AndExpr, andint(X, Y)], ValidatedSoFar) :-
    member([X, Conjunct1, _], ValidatedSoFar),
    member([Y, Conjunct2, _], ValidatedSoFar), 
    AndExpr = and(Conjunct1, Conjunct2),
    write("andint"), nl.

% Conjunction Elimination Left (∧E₁)
valid_line(_,_,[LineNr, AndExpr, andel1(X)], ValidatedSoFar) :-
    member([X, and(Conjunct1, Conjunct2), _], ValidatedSoFar),
    AndExpr = Conjunct1,
    write("andel1"), nl.

% Conjunction Elimination Right (∧E₂)
valid_line(_,_,[LineNr, AndExpr, andel2(X)], ValidatedSoFar) :-
    member([X, and(Conjunct1, Conjunct2), _], ValidatedSoFar),
    AndExpr = Conjunct2,
    write("andel2"), nl.

% Disjunction Introduction Left (∨I₁)
valid_line(_,_,[LineNr, OrExpr, orint1(X)], ValidatedSoFar) :-
    member([X, Disjunct1, _], ValidatedSoFar),
    OrExpr = or(Disjunct1, Disjunct2),
    write("orint1"), nl.

% Disjunction Introduction Right (∨I₂)
valid_line(_,_,[LineNr, OrExpr, orint2(X)], ValidatedSoFar) :-
    member([X, Disjunct2, _], ValidatedSoFar),
    OrExpr = or(Disjunct1, Disjunct2),
    write("orint2"), nl.

% Helper predicate to find a box (subproof) by start and end line numbers
find_box(ValidatedSoFar, StartLine, EndLine, Box) :-
    % write("find_box : "), write(StartLine), write(EndLine), nl,
    member(Box, ValidatedSoFar),

    Box = [[StartLine, _, assumption] |_],
    write("Checking for box: "), write(Box), nl,

    write("Validated: "), write(ValidatedSoFar), nl, 
    write("Box found in validated: "), write(Box), nl, 


    member([EndLine, _, _], Box),
    write("found box"), nl.

% Disjunction Elimination (∨E)
valid_line(_, _, [LineNr, OrelExpr, orel(OrExprLine, StartFirst, EndFirst, StartSecond, EndSecond)], ValidatedSoFar) :-
    OrExpr = or(LeftDisjunct, RightDisjunct),
    write("Orexpr: "), write(OrExpr), nl, 
    member([OrExprLine, OrExpr, _], ValidatedSoFar),

    write("find box1..."), nl,
    find_box(ValidatedSoFar, StartFirst, EndFirst, Box1), 
    Box1 = [[StartFirst, LeftDisjunct, assumption] | _],
    % member([EndFirst, OrelExpr, _], Box1),
    last([EndFirst, OrelExpr, _], Box1),
    write("BOx 1: "), write(Box1), nl,
    
    write("find box2..."), nl,
    find_box(ValidatedSoFar, StartSecond, EndSecond, Box2),
    Box2 = [[StartSecond, RightDisjunct, assumption] | _],
    % member([EndSecond, OrelExpr, _], Box2),
    last([EndSecond, OrelExpr, _], Box2),
    write("BOx 2: "), write(Box2), nl,

    write("orel"), nl.

% Negation Introduction (¬I) -> need box
valid_line(_,_,[LineNr, neg(Negand), negint(X, Y)], ValidatedSoFar) :-
    % member([X, Negand, assumption], Box),
    % member([Y, cont, _], Box),
    find_box(ValidatedSoFar, X, Y, Box), 
    member([X, Negand, assumption], Box),
    % member([Y, cont, _], Box),
    last([Y, cont, _], Box),
    write("negint"), nl. 

% Negation Elimination (¬E)
valid_line(_,_,[LineNr, NegExpr, negel(X, Y)], ValidatedSoFar) :-
    member([X, Negand, _], ValidatedSoFar),
    member([Y, neg(Negand), _], ValidatedSoFar),
    NegExpr = cont,
    write("negel"), nl.

% Double Negation Elimination (¬¬E)
valid_line(_,_,[LineNr, DoubleNegExpr, negnegel(X)], ValidatedSoFar) :-
    member([X, neg(neg(Expr)), _], ValidatedSoFar),
    DoubleNegExpr = Expr,
    write("negnegel"), nl.

% Double Negation Introduction
valid_line(_,_, [LineNr, Expr, negnegint(X)], ValidatedSoFar) :-
    member([X, Negand, _], ValidatedSoFar),
    Expr = neg(neg(Negand)),
    write("negnegint"), nl.

% Contradiction Elimination (⊥e)
valid_line(_,_, [LineNr, Expr, contel(X)], ValidatedSoFar) :-
    member([X, cont, _], ValidatedSoFar),
    write("contel"), nl.

% Modus Tollens (MT)
valid_line(_,_,[LineNr, MtExpr, mt(X, Y)], ValidatedSoFar) :-
    member([X, imp(Antecedent, Conclusion), _], ValidatedSoFar),
    member([Y, neg(Conclusion), _], ValidatedSoFar),
    MtExpr = neg(Antecedent),
    write("mt"), nl.

% PBC rule -> need box
valid_line(_,_, [LineNr, Expr, pbc(X, Y)], ValidatedSoFar) :-
    % member([X, neg(Expr), assumption], Box), 
    % member([Y, cont, _], Box),
    find_box(ValidatedSoFar, X, Y, Box), 
    member([X, neg(Expr), assumption], Box), 
    % member([Y, cont, _], Box),
    last([Y, cont, _], Box),
    write("pbc"), nl.

% Law of the Excluded Middle (LEM)
valid_line(_,_,[LineNr, or(LemExpr, neg(LemExpr)), lem], ValidatedSoFar) :-
    write("lem"), nl.

% verify -> read file
verify(InputFileName) :- 
    see(InputFileName),     % open file with InputFileName
    read(Prems), read(Goal), read(Proof),            
    seen,                   % close file
    write(Prems), nl,
    write(Goal), nl,
    write(Proof), nl, nl,
    nl,
    (valid_proof(Prems, Goal, Proof) -> write("yes"), nl ; write("no"), nl).
