% This requires Hyprolog
%:-consult(hyprolog).
assumptions ref/1, proof/1, count/1, missing/1.
:- dynamic given_true/1, given_false/1.

% ====== Database: ======
disease(X):-member(X, [cold, influenza, malaria, measles, mumps]).
symptom(X):-member(X, [aches, anemia, coughing, fatigue, fever, headache, nasal_congestion, rash, red_eyes, runny_nose, shivering, sore_throat, swelling, vomiting]).
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
have(A, B):-has(A, B);had(A, B).

are_sick(P):- (have(P,X);had(P,X)),disease(X).

% Symptoms list
symptom_of(cold, S):-member(S, [coughing, nasal_congestion, runny_nose, sore_throat]).
symptom_of(influenza, S):-member(S, [aches, fatigue, fever, headache]).
symptom_of(malaria, S):-member(S, [anemia, fever, shivering, vomiting]).
symptom_of(measles, S):-member(S, [coughing, runny_nose, red_eyes, red_rash]).
symptom_of(mumps, S):-member(S, [fever, headache, swelling]).

causes(D, S) :- symptom_of(D, S).

% Disease rules:
problem(cold) :- coughing, nasal_congestion, runny_nose, sore_throat.
problem(influenza) :- aches, fatigue, fever, headache.
problem(malaria) :- anemia, fever, shivering, vomiting.
problem(measles) :- coughing, runny_nose, red_eyes, red_rash.
problem(mumps) :- fever, headache, swelling.

rule(problem(cold), [coughing, nasal_congestion, runny_nose, sore_throat]).
rule(problem(influenza), [aches, fatigue, fever, headache]).
rule(problem(malaria), [anemia, fever, shivering, vomiting]).
rule(problem(measles), [coughing, runny_nose, red_eyes, red_rash]).
rule(problem(mumps), [fever, headache, swelling]).

hot(_).
bad(_).
sick(_).
high(_).

% Askable questions:
askable(X):-symptom(X).

% Predicates to help with answering queries:
the(_,A,B):-call(A),call(B).
a(_,A,B):-call(A),call(B).
defined_is(A,A).
and(A, B) :- call(A), call(B).
how_many(X, M, Length) :- setof(X, M, Result), length(Result, Length).%, +count(Length).

% Useful display predicates:
display_list([], _) :- nl.
display_list([X], _) :- write(X), write('.'), nl, !.
display_list([X, Y], L) :- write(X), (L<3;write(',')), write(' and '), write(Y), write('.'), nl,!.
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

query(Problem) --> [what,is,the,problem,'?'],{solve(problem(Problem))}.
query(setof(X, Meaning, Results)) --> [what,are],fragment(X, Meaning),['?'],{display_list(Results)}.

% Sentence fragment:
fragment(Subject, Meaning) --> nounPhrase(Subject, _, Meaning).
fragment(Subject, Meaning) --> nounPhrase(Subject, VerbPhrase, Meaning), verbPhrase(Subject, VerbPhrase).

% Verbs:
verbPhrase(NounPhrase, Meaning) --> verb(NounPhrase, VerbPhrase, L), complements(L, VerbPhrase, Meaning).

verb(Subject, defined_is(Subject, Subject2), [['FIRST', Subject2]]) --> ['is'].
verb(Subject, as(Subject, Subject2), [['FIRST', Subject2]]) --> [as].
verb(Subject, had(Subject, Object), [['FIRST', Object]]) --> [had].
verb(Subject, has(Subject, Object), [['FIRST', Object]]) --> [has].
verb(Subject, have(Subject, Object), [['FIRST', Object]]) --> [have].
verb(Subject, causes(Subject, Subject2), [['FIRST', Subject2]]) --> [causes];[cause].
verb(Subject, cured(Subject), []) --> [cured].
verb(Subject, cures(Subject, Subject2), [['FIRST', Subject2]]) --> [cures].
verb(Subject, transmitted(Subject, Object), [['FIRST', Object]]) --> [transmitted].
verb(Subject, transmitted(Subject, Object, Subject2), [['FIRST', Object], [to, Subject2]]) --> [transmitted].
verb(Subject, got(Subject, Object), [['FIRST', Object]]) --> [got].
verb(Subject, got(Subject, Object, Subject2), [['FIRST', Object], [from, Subject2]]) --> [got].
verb(Subject, gave(Subject, Object, Subject2), [['FIRST', Object], [to, Subject2]]) --> [gave].
verb(Subject, symptom_of(Object, Subject), [['FIRST', Object]]) --> [symptom], [of].
verb(Subject, symptom_of(Object, Subject), [['FIRST', Object]]) --> [symptoms], [of].
verb(Subject, are_sick(Subject), []) --> [are,sick];[are,ill].

% Noun phrase:
nounPhrase(Subject, Meaning, Meaning) --> properNoun(Subject).
nounPhrase(Subject, VerbPhrase, Meaning) --> whWord(Subject, NounPhrase, Meaning), nounPhrase(Subject, VerbPhrase, NounPhrase), !.
nounPhrase(Subject, VerbPhrase, Meaning) --> whWord(Subject, VerbPhrase, Meaning).
nounPhrase(Subject, VerbPhrase, Meaning) --> determiner(Subject, AdjPhrase, VerbPhrase, Meaning), adjective(Subject, NounPhrase, AdjPhrase), commonNoun(Subject, Noun), relativeClause(Subject, Noun, NounPhrase).

nounPhrase(Subject, Meaning, Meaning) --> {-missing(Subject)}.

% Proper nouns: (names)
properNoun(X) --> [X],{disease(X);symptom(X);name(X);member(X,[medicine, cure])}.

% Who nouns:
whWord(Who,M,M) --> [who],{+ref(Who)}.
whWord(What,M,M) --> [what],{+ref(What)}.
whWord(What,M,M) --> [what,are], {+ref(What)}.
whWord(People,M,M) --> [people], {+ref(People)}.

whWord(Subject,VP,and(disease(Subject), VP)) --> [disease],{+ref(Subject)}.
whWord(Subject,VP,and(disease(Subject), VP)) --> [diseases],{+ref(Subject)}.
whWord(Subject,VP,and(symptom(Subject), VP)) --> [symptom],{+ref(Subject)}.
whWord(Subject,VP,and(symptom(Subject), VP)) --> [symptoms],{+ref(Subject)}.

whWord(He, VerbPhrase, and(he(He),VerbPhrase)) --> [he],{+ref(He)}.
whWord(She, VerbPhrase, and(she(She), VerbPhrase)) --> [she],{+ref(She)}.

whWord(Subject, VerbPhrase, how_many(Subject, Meaning, C)) --> [how,many], nounPhrase(Subject, VerbPhrase, Meaning), {+count(C)}.
whWord(HowMany, VerbPhrase, how_many(HowMany, VerbPhrase, C)) --> [how,many], {+count(C)}.

% Common nouns:
commonNoun(X, flu(X)) --> [flu].
commonNoun(X, cold(X)) --> [cold].
commonNoun(X, measles(X)) --> [measles].
commonNoun(X, mumps(X)) --> [mumps].
commonNoun(X, doctor(X)) --> [doctor].
commonNoun(X, patient(X)) --> [patient].
commonNoun(X, nurse(X)) --> [nurse].
commonNoun(X, solve(problem(X))) --> [problem].
commonNoun(X, symptom(X)) --> [symptoms].
commonNoun(X, disease(X)) --> [disease];[diseases].

% Plural common nouns:
pluralCommonNoun(X, disease(X)) --> [diseases].
pluralCommonNoun(X, doctor(X)) --> [doctors].
pluralCommonNoun(X, patient(X)) --> [patients].
pluralCommonNoun(X, nurse(X)) --> [nurses].

% Determiners:
determiner(Subject, NounPhrase, VerbPhrase, the(Subject, NounPhrase, VerbPhrase)) --> [the].
determiner(Subject, NounPhrase, VerbPhrase, a(Subject, NounPhrase, VerbPhrase)) --> [a].
determiner(Subject, NounPhrase, VerbPhrase, of(Subject, NounPhrase, VerbPhrase)) --> [of].

% Relative clauses:
relativeClause(Subject, Noun, and(Noun, R)) --> [of],{+missing(Subject)}, fragment(_, R).
relativeClause(_,Noun,Noun) --> [].

% Adjectives
adjective(_, NounPhrase, NounPhrase) --> [].
adjective(Subject, NounPhrase, and(NounPhrase, Meaning)) --> [Adj], {member(Adj,[hot,high,sick,bad]), Meaning=..[Adj,Subject]}.
adjective(Subject, NounPhrase, and(NounPhrase, Meaning)) --> [Adj], {member(Adj,[hot,high,sick,bad]), AdjPhrase=..[Adj,Subject]}, adjective(Subject, AdjPhrase, Meaning).

% Solver for answering queries: starter:
solve(A) :- retractall(given_true(_)), retractall(given_false(_)),
            write('Please answer yes or no to the following list of symptoms:'), nl,
            solve(A, [], _).

% Normal solver
solve(true, _, true) :- !.
solve(not(A), Rules, not(ProofA)) :- not(solve(A, Rules, ProofA)).
solve((A, B), Rules, (ProofA, ProofB)) :- !, solve(A, Rules, ProofA), solve(B, Rules, ProofB).
solve(A, Rules, (A:-ProofB)) :- clause(A, B), solve(B, [(A:-B)|Rules], ProofB).

% Check if we know about this already
solve(A, _, (A:-given)) :- given_true(A), !, write('Symptom already listed: '), write(A), nl.
solve(A, _, _) :- given_false(A), !, write('Symptom already denied: '), write(A), nl, fail.

% Otherwise, ask the user
solve(A, Rules, (A:-given)) :- askable(A), ((ask_user(A, Rules), assert(given_true(A)), write('Given as true: '), write(A), nl, !);
                                            (assert(given_false(A)), write('Given as false: '), write(A), nl, !, fail)).

ask_user(A, Rules):- write(A), write('?'), nl,
                    read(Answer), ask_respond(Answer, A, Rules).
ask_respond(true, _, _).
ask_respond(why, A, [Rule|Rules]):- write(Rule), nl, ask_user(A, Rules).
ask_respond(why, A, []):- ask_user(A, []).

% Responder:
respond(Meaning) :- call(Meaning), -count(C), write(C), write('.'), nl, !.
respond(Meaning) :- -ref(X), setof(X, Meaning, Result), display_list(Result), !.

