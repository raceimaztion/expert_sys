% This requires Hyprolog
:-consult(hyprolog).
assumptions ref/1, proof/1.

% ====== Database: ======
disease(X):-member(X, [cold, influenza, malaria, measles, mumps]).
symptom(X):-member(X, [aches, anaemia, coughing, fatigue, fever, headache, 'nasal congestion', rash, 'red eyes', 'runny nose', shivering, 'sore throat', swelling, vomiting]).
medicine(X):-member(X, [coldFX, 'vitamin C', echinacea]).
he(X):-member(X, [john, bob, steff]).
she(X):-member(X, [mary, lydia]).
name(X):-he(X);she(X).

% Used for "a cold", "the flu", "the measles", and "the mumps"
cold(cold).
flu(influenza).
measles(measles).
mumps(mumps).

% Who are doctors, patients and nurse(s)
doctor(steff).
patient(X):-member(X, [john, bob, mary]).
nurse(lydia).

had(bob, influenza).
has(john, cold).
has(D, P):-doctor(D),patient(P).
has(P, D):-patient(P),doctor(D).
has(D, N):-doctor(D),nurse(N).

% Symptoms list
symptom_of(cold, S):-member(S, [coughing, nasal_congestion, runny_nose, sore_throat]).
symptom_of(influenza, S):-member(S, [aches, fatigue, fever, headache]).
symptom_of(malaria, S):-member(S, [anemia, fever, shivering, vomiting]).
symptom_of(measles, S):-member(S, [coughing, runny_nose, red_eyes, red_rash]).
symptom_of(mumps, S):-member(S, [fever, headache, swelling]).

% Disease rules:
problem(cold) :- coughing, nasal_congestion, runny_nose, sore_throat.
problem(influenza) :- aches, fatigue, fever, headache.
problem(malaria) :- anemia, fever, shivering, vomiting.
problem(measles) :- coughing, runny_nose, red_eyes, red_rash.
problem(mumps) :- fever, headache, swelling.

% Predicates to help with answering queries:
the(_,A,B):-call(A),call(B).
a(_,A,B):-call(A),call(B).

% Useful display predicates:
display_list([], _) :- nl.
display_list([X], _) :- write(X), write('.'), nl, !.
display_list([X, Y], L) :- write(X), (L<3;write(',')), write(' and '), write(Y), nl,!.
display_list([X|List], L) :- write(X), write(', '), display_list(List, L).
display_list(List) :- length(List, Length), display_list(List, Length).

% Complements:
complements([],Meaning,Meaning) --> [].
complements([C|L],Meaning1,Meaning) --> complement(C,Meaning1,Meaning2), complements(L,Meaning2,Meaning).

complement([PrepPhrase,Subject], Meaning1, Meaning) --> preposition(PrepPhrase), nounPhrase(Subject, Meaning1, Meaning).

% Prepositions:
preposition('FIRST') --> [].
preposition(X) --> [X],{preposition(X)}.
preposition(X):-member(X,[about, above, across, after, against, along, among, around, at, before, behind, below, beneath, beside, between, beyond, but, by, despite, down, during, except, for, from, in, inside, into, like, near, of, off, on, onto, out, outside, over, past, since, through, throughout, till, to, toward, under, underneath, until, up, upon, with, within, without]).

% Sentence fragment:
fragment(Meaning) --> nounPhrase(Subject, VerbPhrase, Meaning), verbPhrase(Subject, VerbPhrase).

% Verbs:
verbPhrase(NounPhrase, Meaning) --> verb(NounPhrase, VerbPhrase, L), complements(L, VerbPhrase, Meaning).

verb(Subject, (is(Subject, Subject2)), [['FIRST', Subject2]]) --> ['is'].
verb(Subject, as(Subject, Subject2), [['FIRST', Subject2]]) --> [as].
verb(Subject, had(Subject, Object), [['FIRST', Object]]) --> [had].
verb(Subject, has(Subject, Object), [['FIRST', Object]]) --> [has].
verb(Subject, have(Subject, Object), [['FIRST', Object]]) --> [have].
verb(Subject, causes(Subject, Subject2), [['FIRST', Subject2]]) --> [causes].
verb(Subject, cured(Subject), []) --> [cured].
verb(Subject, cures(Subject, Subject2), [['FIRST', Subject2]]) --> [cures].
verb(Subject, transmitted(Subject, Object), [['FIRST', Object]]) --> [transmitted].
verb(Subject, transmitted(Subject, Object, Subject2), [['FIRST', Object], [to, Subject2]]) --> [transmitted].
verb(Subject, got(Subject, Object), [['FIRST', Object]]) --> [got].
verb(Subject, got(Subject, Object, Subject2), [['FIRST', Object], [from, Subject2]]) --> [got].
verb(Subject, gave(Subject, Object, Subject2), [['FIRST', Object], [to, Subject2]]) --> [gave].
verb(Subject, symptom_of(Object, Subject), [['FIRST', Object]]) --> [symptom], [of].

% Noun phrase:
nounPhrase(Subject, Meaning, Meaning) --> properNoun(Subject).
nounPhrase(Subject, VerbPhrase, Meaning) --> determinant(Subject, NounPhrase, VerbPhrase, Meaning), pronoun(Subject, NounPhrase).

% Proper nouns: (names)
properNoun(X) --> [X],{disease(X);symptom(X);name(X);member(X,[medicine, cure, symptom, symptoms, disease])}.

% "Improper" nouns:
properNoun(Who) --> [who],{+ref(Who)}.
properNoun(He) --> [he],{+ref(He)}.
properNoun(She) --> [she],{+ref(She)}.
properNoun(What) --> [what],{+ref(What)}.

% Pronouns:
pronoun(X, flu(X)) --> [flu].
pronoun(X, cold(X)) --> [cold].
pronoun(X, measles(X)) --> [measles].
pronoun(X, mumps(X)) --> [mumps].
pronoun(X, doctor(X)) --> [doctor].
pronoun(X, patient(X)) --> [patient].
pronoun(X, nurse(X)) --> [nurse].
pronoun(X, solve(problem(X), [], Proof)) --> [problem], {+proof(Proof)}.

% Determinants:
determinant(Subject, NounPhrase, VerbPhrase, the(Subject, NounPhrase, VerbPhrase)) --> [the].
determinant(Subject, NounPhrase, VerbPhrase, a(Subject, NounPhrase, VerbPhrase)) --> [a].

% Adjectives



% Solver for answering queries
solve(true, _, true) :- !.
solve(not(A), Rules, not(ProofA)) :- not(solve(A, Rules, ProofA)).
solve((A, B), Rules, (ProofA, ProofB)) :- !, solve(A, Rules, ProofA), solve(B, Rules, ProofB).
solve(A, Rules, (A:-ProofB)) :- clause(A, B), solve(B, [(A:-B)|Rules], ProofB).
solve(A, Rules, (A:-given)) :- ask_user(A, Rules).

ask_user(A, Rules):- write(A), write('? Enter true if the goal is true, false otherwise'), nl,
                    read(Answer), ask_respond(Answer, A, Rules).
ask_respond(true, _, _).
ask_respond(why, A, [Rule|Rules]):- write(Rule), nl, ask_user(A, Rules).
ask_respond(why, A, []):- ask_user(A, []).

% Responder:
respond(Meaning):- -ref(X), setof(X, Meaning, Result), display_list(Result).

