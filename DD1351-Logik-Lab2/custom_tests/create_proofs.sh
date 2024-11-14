#!/bin/bash

# Create Implication Elimination (impel.proof)
cat <<EOL > impel.proof
**Premises:**
[imp(p, q), p].

**Conclusion:**
q.

**Proof:**
[
  [1, imp(p, q), premise],
  [2, p,        premise],
  [3, q,        impel(2,1)]
].
EOL

# Create Negation Introduction (negint.proof)
cat <<EOL > negint.proof
**Premises:**
[imp(p, q), neg(q)].

**Conclusion:**
neg(p).

**Proof:**
[
  [1, imp(p, q),    premise],
  [2, neg(q),       premise],
  [
    [3, p,          assumption],
    [4, q,          impel(3,1)],
    [5, cont,       negel(4,2)]
  ],
  [6, neg(p),       negint(3,5)]
].
EOL

# Create Negation Elimination (negel.proof)
cat <<EOL > negel.proof
**Premises:**
[p, neg(p)].

**Conclusion:**
cont.

**Proof:**
[
  [1, p,       premise],
  [2, neg(p),  premise],
  [3, cont,    negel(1,2)]
].
EOL

# Create Contradiction Elimination (contel.proof)
cat <<EOL > contel.proof
**Premises:**
[p, neg(p)].

**Conclusion:**
q.

**Proof:**
[
  [1, p,       premise],
  [2, neg(p),  premise],
  [3, cont,    negel(1,2)],
  [4, q,       contel(3)]
].
EOL

# Create Double Negation Introduction (negnegint.proof)
cat <<EOL > negnegint.proof
**Premises:**
[p].

**Conclusion:**
neg(neg(p)).

**Proof:**
[
  [1, p,              premise],
  [2, neg(neg(p)),    negnegint(1)]
].
EOL

# Create Double Negation Elimination (negnegel.proof)
cat <<EOL > negnegel.proof
**Premises:**
[neg(neg(p))].

**Conclusion:**
p.

**Proof:**
[
  [1, neg(neg(p)),  premise],
  [2, p,            negnegel(1)]
].
EOL

# Create Modus Tollens (mt.proof)
cat <<EOL > mt.proof
**Premises:**
[imp(p, q), neg(q)].

**Conclusion:**
neg(p).

**Proof:**
[
  [1, imp(p, q),  premise],
  [2, neg(q),     premise],
  [3, neg(p),     mt(1, 2)]
].
EOL

# Create Proof by Cases (pcb.proof)
cat <<EOL > pcb.proof
**Premises:**
[or(p, q), imp(p, r), imp(q, r)].

**Conclusion:**
r.

**Proof:**
[
  [1, or(p, q),      premise],
  [2, imp(p, r),     premise],
  [3, imp(q, r),     premise],
  [
    [4, p,           assumption],
    [5, r,           impel(4,2)]
  ],
  [
    [6, q,           assumption],
    [7, r,           impel(6,3)]
  ],
  [8, r,             pcb(1,4,5,6,7)]
].
EOL

# Create Law of Excluded Middle (lem.proof)
cat <<EOL > lem.proof
**Premises:**
[].

**Conclusion:**
or(p, neg(p)).

**Proof:**
[
  [1, or(p, neg(p)), lem]
].
EOL

echo "All proof files have been created successfully."
