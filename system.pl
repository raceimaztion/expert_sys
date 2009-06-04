% This requires Hyprolog
:-consult(hyprolog).
assumptions missing/1, who/1, he/1, she/1.

% Database:
disease(X):-member(X, [cold, influenza, malaria, measles, mumps]).
symptom(X):-member(X, [aches, anaemia, coughing, fatigue, fever, headache, 'nasal congestion', rash, 'red eyes', 'runny nose', shivering, 'sore throat', swelling, vomiting]).
medicine(X):-member(X, [coldFX, 'vitamin C', echinacea]).

cold(cold).
flu(influenza).
measles(measles).
mumps(mumps).

symptom_of(cold, S):-member(S, [coughing, 'nasal congestion', 'runny nose', 'sore throat']).
symptom_of(flu, S):-member(S, [aches, fatigue, fever, headache]).
symptom_of(malaria, S):-member(S, [anemia, fever, shivering, vomiting]).
symptom_of(measles, S):-member(S, [coughing, 'runny nose', 'red eyes', 'red rash']).
symptom_of(mumps, S):-member(S, [fever, headache, swelling]).

% Complements:
complements([],Meaning,Meaning) --> [].
complements([C|L],Meaning1,Meaning) --> complement(C,Meaning1,Meaning2), complements(L,Meaning2,Meaning).

complement([PrepPhrase,Subject], Meaning1, Meaning) --> preposition(PrepPhrase), nounPhrase(Subject, Meaning1, Meaning).

% Prepositions:
preposition('FIRST') --> [].
preposition(X) --> [X],{preposition(X)}.
preposition(about).
preposition(above).
preposition(across).
preposition(after).
preposition(against).
preposition(along).
preposition(among).
preposition(around).
preposition(at).
preposition(before).
preposition(behind).
preposition(below).
preposition(beneath).
preposition(beside).
preposition(between).
preposition(beyond).
preposition(but).
preposition(by).
preposition(despite).
preposition(down).
preposition(during).
preposition(except).
preposition(for).
preposition(from).
preposition(in).
preposition(inside).
preposition(into).
preposition(like).
preposition(near).
preposition(of).
preposition(off).
preposition(on).
preposition(onto).
preposition(out).
preposition(outside).
preposition(over).
preposition(past).
preposition(since).
preposition(through).
preposition(throughout).
preposition(till).
preposition(to).
preposition(toward).
preposition(under).
preposition(underneath).
preposition(until).
preposition(up).
preposition(upon).
preposition(with).
preposition(within).
preposition(without).

% Sentence fragment:
fragment(Meaning) --> nounPhrase(Subject, VerbPhrase, Meaning), verbPhrase(Subject, VerbPhrase).

% Verbs:
verbPhrase(NounPhrase, Meaning) --> verb(NounPhrase, VerbPhrase, L), complements(L, VerbPhrase, Meaning).

verb(Subject, is(Subject, Subject2), [['FIRST', Subject2]]) --> ['is'].
verb(Subject, as(Subject, Subject2), [['FIRST', Subject2]]) --> [as].
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

% Noun phrase:
nounPhrase(Subject, Meaning, Meaning) --> properNoun(Subject).
nounPhrase(Subject, VerbPhrase, Meaning) --> determinant(Subject, NounPhrase, VerbPhrase, Meaning), pronoun(Subject, NounPhrase).

% Proper nouns: (names)
properNoun(X) --> [X],{disease(X);symptom(X);member(X, [medicine, cure, symptom, symptoms, disease])}.

% "Improper" nouns:
properNoun(Who) --> [who],{+who(Who)}.
properNoun(He) --> [he],{+he(He)}.
properNoun(She) --> [she],{+she(She)}.

% Pronouns:
pronoun(X, flu(X)) --> [flu].
pronoun(X, cold(X)) --> [cold].
pronoun(X, measles(X)) --> [measles].
pronoun(X, mumps(X)) --> [mumps].

% Determinants:
determinant(Subject, NounPhrase, VerbPhrase, the(Subject, NounPhrase, VerbPhrase)) --> [the].
determinant(Subject, NounPhrase, VerbPhrase, a(Subject, NounPhrase, VerbPhrase)) --> [a].
