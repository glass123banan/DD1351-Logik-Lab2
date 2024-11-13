% propositioner
imp(P, Q)   % p -> q
neg(P)      % !p
and(P, Q)   % p n q
or(P, Q)    % p v q

% check if step belongs to prems
valid_line(Prems, Goal, Step) :-
    member(Step, Prems). 

% check some rule
valid_line(Prems, Goal, Step) :-
    % todo



% valid proof check
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
    valid_proof(Prems, Goal, Proof).    % check if proof is valid

