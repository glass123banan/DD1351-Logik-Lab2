% [LineNr, Expr, Rule]
% check if step belongs to prems
valid_line(Prems, _, [LineNr, Expr, Premise], _) :-
    member(Expr, Prems). 

% check if line belongs to goal 
valid_line(_, Goal, [LineNr, Expr, _], _) :-
    member(Expr, Goal).

% check if line uses andint
valid_line(_, _, (A, B), ValidatedSoFar) :-
    member(A, ValidatedSoFar),
    member(B, ValidatedSoFar).

% check if line uses andel

% helper func that stores checked lines in ValidatedSoFar
valid_proof_validator(Prems, Goal, [Step | RestOfProof], ValidatedSoFar) :-
    valid_line(Prems, Goal, Step, [Step | RestOfProof]), 
    valid_proof_validator(Prems, Goal, RestOfProof, [Step | ValidatedSoFar]).


% valid proof check recursive case
valid_proof(Prems, Goal, Proof) :-
    valid_proof_validator(Prems, Goal, Proof, []).

% verify -> read file
verify(InputFileName) :- 
    see(InputFileName),     % open file with InputFileName
    read(Prems),            % read premises
    read(Goal),             % read goals
    read(Proof),            % read proof steps
    seen,                   % close file
    valid_proof(Prems, Goal, Proof).    % check if proof is valid

