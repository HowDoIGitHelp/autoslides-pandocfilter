class: center, middle

## Logic Programming Paradigm

---
class: center, middle

## Introduction

---

## Learning Outcomes

1.  Create Prolog facts, rules, and queries
2.  Explain the process of unification
3.  Explain how proof search is used to respond to queries
4.  Create recursive Prolog rules

---

## Facts

Here's an example of a knowledge base:

``` prolog
firetype(charmander).
firetype(charizard).
watertype(squirtle).
flyingtype(charizard).
```

---

## Facts

One of those are:

``` prolog
firetype(charmander).
```

---

## Facts

For example:

``` prolog
this_is_a_fact.
axiom_1.
```

---

## Facts

A fact like, `firetype(charmander)` can be written in the form of an
implication as such:

`$$
\begin{aligned}
\text{firetype}(\text{charmander}) & \equiv \text{firetype}(\text{charmander}) \lor \bot\\
& \equiv \top \to \text{firetype}(\text{charmander}) \\
\end{aligned}
$$`

---

## Rules

``` prolog
firetype(charmander).
firetype(charizard).
watertype(squirtle).
flyingtype(charizard).

resistanttofire(squirtle) :- watertype(squirtle).
```

---

## Rules

`$$
\begin{aligned}
\text{watertype}(\text{squirtle}) \to \text{resistanttofire}(\text{squirtle}) & \equiv \\
\neg \text{watertype}(\text{squirtle}) \lor \text{resistanttofire}(\text{squirtle})
\end{aligned}
$$`

---

## Queries

``` prolog
?- firetype(charmander).
```

---

## Queries

Therefore, it responds with:

``` prolog
true.
```

---

## Queries

``` prolog
?- firetype(squirtle).
```

---

## Queries

Realizing that none of the facts match this proposition, Prolog responds
with:

``` prolog
false.
```

---

## Queries

``` prolog
?- firetype(charizard), watertype(squirtle).
```

---

## Queries

``` prolog
true.
```

---

## Queries

For example, the query `firetype(charizard), watertype(squirtle)`, is
negated into the disjunction:

`$$
\begin{aligned}
\neg \text{firetype}(\text{charizard}) \lor \neg \text{watertype}(\text{squirtle})
\end{aligned}
$$`

---

## Queries

Here's an example, given the knowledge base:

``` prolog
p.
q.
r :- q.
```

---

## Queries

And the query:

``` prolog
?- r.
```

---

## Queries

`$$
\begin{aligned}
(p) \land \\
(q) \land \\
(\neg q \lor r) \land \\
(\neg r)
\end{aligned}
$$`

---

## Queries

We then check if resulting Horn formula is satisfiable:

`$$
\begin{aligned}
(\top) \land \\
(\top) \land \\
(\bot \lor r) \land \\
(\neg r)
\end{aligned}
$$`

---

## Queries

`$$
\begin{aligned}
(r) \land \\
(\neg r)
\end{aligned}
$$`

---

## Queries

`$$
\begin{aligned}
(\top) \land \\
(\bot)
\end{aligned}
$$`

---

## Queries

``` prolog
true.
```

---

## Variables

``` prolog
pokemon(X).
```

---

## Variables

``` prolog
?- pokemon(charizard).
```

---

## Variables

``` prolog
true.
```

---

## Variables

``` prolog
firetype(charmander).
firetype(charizard).
watertype(squirtle).
flyingtype(charizard).

resistanttofire(squirtle) :- watertype(squirtle).
```

---

## Variables

``` prolog
?- firetype(X)
```

---

## Variables

This can be interpreted in natural language as "which Pokémon are fire
type?" Therefore, this query will yield the response:

``` prolog
X = charmander
X = charizard
```

---

## Variables

Instead of the rule `resistanttofire(squirtle) :- watertype(squirtle).`
we can write a more general rule using variables:

``` prolog
firetype(charmander).
firetype(charizard).
watertype(squirtle).
flyingtype(charizard).
```

---

## Variables

``` prolog

isresistantto(X,Y) :- watertype(X),firetype(Y).
isresistantto(X,Y) :- watertype(X),watertype(Y).
```

---

## Variables

> for all pairs of X and Y, X is resistant to Y, if X is water type and
> Y is fire type,

---

## Variables

This statement, can be written as the following quantification
statement:

- By writing this rule, Prolog can infer the following facts:

---

## Variables

``` prolog
?- isresistantto(squirtle,charmander).
true.
```

---

## Variables

``` prolog
?- isresistantto(squirtle,charizard).
true.
```

---

## Variables

``` prolog
?- isresistantto(squirtle,squirtle).
true.
```

---

## Variables

If you ask Prolog a harder question like the following:

``` prolog
?- isresistantto(squirtle,X).
```

---

## Variables

Therefore, Prolog will look for the pokémon, squirtle is resistant to,
therefore you with the output:

``` prolog
X = charmander
X = charizard
X = squirtle
```

---

## Variables

``` prolog
?- isresistantto(X,Y).
X = squirtle,
Y = charmander ;
X = squirtle,
Y = charizard ;
X = Y, Y = squirtle.
```

---

## Variables

``` prolog
p(a).
q(a).
z(X) :- p(X), q(X).

?- z(a).
```

---

## Variables

Writing the knowledge base and the negation of the query as clauses:

`$$
\begin{aligned}
p(a) \land \\
q(a) \land \\
\neg \forall X p(X) \lor \neg \forall X q(X) \lor \forall X z(X) \\
\neg z(a)
\end{aligned}
$$`

---

## Variables

`$$
\begin{aligned}
p(a) \land \\
q(a) \land \\
\neg \forall X p(X) \lor \neg \forall X q(X)
\end{aligned}
$$`

---

## Variables

`$$
\begin{aligned}
q(a) \land \\
\neg \forall X q(X)
\end{aligned}
$$`

---

## Variables

Apply universal instantiation to $\forall X q(X)$:

`$$
\begin{aligned}
q(a) \land \\
\neg q(a)
\end{aligned}
$$`
