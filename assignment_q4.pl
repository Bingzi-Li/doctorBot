:-[conversing].
:- dynamic pain/1.
:- dynamic mood/1.
:- dynamic have/1.
:- dynamic nothave/1.
:- dynamic diagnosis/1.

/* beginnig of questions */
ask(0):-	pain_library(P), write('Nice to meet to my friend. What\' s up today? '), ask_pain(P).

ask_pain([PH|PT]):-
    opening(OP),
    question_start(QS), 
    write(OP), write(QS), to_string(PH, PHS), write(PHS),
    write('? y/n/q: '), 
    read(HasPain), 
    (
        HasPain==q -> abort;
        HasPain==y -> assert(pain(PH)), assert(have(PH)), mood_library(M), ask_mood(M); 
        HasPain==n -> assert(nothave(PH)),(PT==[] -> write('Please tell me how much pain you feel. Take time, let\'s start again.'), ask(0); ask_pain(PT))
    ).

ask_mood([MH|MT]):-
   opening(OP),
   question_start(QS), 
   write(OP), write(QS), to_string(MH, MHS), write(MHS),
   write('? y/n/q: '), 
   read(HasMood), 
   (
       HasMood==q -> abort;
       HasMood==y -> assert(mood(MH)), assert(have(MH)), ask_symptom(0); 
       HasMood==n -> assert(nothave(MH)), (MT==[] -> write('Please tell me about you mood. Don\'t worry, take your time'), mood_library(M), ask_mood(M);  ask_mood(MT))
   ).

/* ask symptoms */
ask_symptom(0):-
    sympathetic(G), write('speaking with '), to_string(G, GS), write(GS), write(': '),
    opening(OP), write(OP), 
    validate_and_query_options([temperature]).

ask_symptom(Y):-    
    diagnose(X), write('My dear patient. Your diagnosis is '), write(X), write('. I hope you get better soon.');
    generate_options(Y,L), validate_and_query_options(L).

generate_options(Y,L):-
    /* select a gesture */
    sympathetic(G), write('speaking with '), to_string(G, GS), write(GS), write(': '), question_start(QS),
    (
        have(Y), write("Ah, I see. "),  write(QS), findnsols(100,X,related(X,Y),L);
        nothave(Y), write("Great. "),  write(QS), findnsols(100,X,random(X),L)
    ).

validate_and_query_options(L):-
	findnsols(100, X, have(X), Havelist), 
    findnsols(100, X, nothave(X), Nothavelist), 
    append(Havelist, Nothavelist, History), 
    list_to_set(L,S), 
    list_to_set(History,H), 
    subtract(S,H,Valid), 
    (
        Valid==[]-> write("alright? I think you are well. Relax, my dear patient. However, if you do feel sick, please approach other doctor. Have a good day! ");
        member(X,Valid), to_string(X, XS), write(XS), write('? y/n/q: '), 
        read(Have), 
        (
            Have==q -> abort;
            Have==y -> assert(have(X));
            Have==n -> assert(nothave(X))
        ), ask_symptom(X)
    ).
   

/* define related symptoms */
related(X,Y):- 
	fever(L),member(X,L),member(Y,L);
	cold(L),member(X,L),member(Y,L);
	injury(L),member(X,L),member(Y,L);
	depression(L),member(X,L),member(Y,L);
	food_poisoning(L),member(X,L),member(Y,L).

/* random select symptoms topic when answered no */
random(X):-
	fever(A), cold(B), injury(C), depression(D), food_poisoning(E), append(A,B,AB), append(AB,C,ABC), append(ABC,D,ABCD), append(ABCD,E,ABCDE), random_member(X,ABCDE).



/* define sympathetic doctor */
sympathetic(G):-
    (
        /* no pain or calm mood, set gesture to normal */
        ( pain(no_pain), mood(calm) ),
        normal_gesture(GL);
        /* mild_pain or manageable_pain or angry, set gesture to polite */
        ( pain(mild_pain); pain(manageable_pain); mood(angry)),
        polite_gesture(GL);
        /* lot_of_pain, unbearable_pain or stressed, weepy, depressed, set gesture to calming */
        ( pain(lot_of_pain); pain(unbearable_pain); mood(stressed); mood(weepy); mood(depressed)),
        calming_gesture(GL)
    ),
    random_member(G, GL). 
    
/* count match for each diagnosis, if one reaches 4, diagnosis is set to that disease */
diagnose(Y):-
    fever(F), cold(C), injury(I), depression(D), food_poisoning(FP), 
    list_to_set(F, FS), list_to_set(C, CS), list_to_set(I, IS), list_to_set(D, DS), list_to_set(FP, FPS),
    findall(X, have(X), L), list_to_set(L, LS),
    (
        intersection(FS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(fever)));
        intersection(CS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(cold)));
        intersection(IS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(injury)));
        intersection(DS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(depression)));
        intersection(FPS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(food_poisoning)))
    ),
    findall(X,diagnosis(X),Diag), diagnosis_library(DL),
    intersection(Diag, DL, Y).

fever([temperature, sweat, ache, weepy, manageable_pain]).
cold([sneeze, cough, temperature, mild_pain, calm]).
injury([blood, lot_of_pain, weepy, angry, bonebreak]).
depression([no_pain, weepy, inactive, depressed, insomnia]).
food_poisoning([stomachache, lot_of_pain, nausea, high_heart_rate, rash]).


pain_library([no_pain, mild_pain, manageable_pain, lot_of_pain, unbearable_pain]).
mood_library([calm, angry, stressed, weepy, depressed]).
diagnosis_library([fever, cold, injury, depression, food_poisoning]).

polite_gesture([look_concerned, mellow_voice, light_touch, faint_smile, eye_contact]).
calming_gesture([greet, look_composed, look_attentive, calming_voice, soft_touch]).
normal_gesture([broad_smile, joke, beaming_voice, energetic_greeting, happy_smile]).

/* select only one pain and one mood */
pain(nothing).
mood(nothing).
/* have a symptom and not have a symptom */
have(nothing).
nothave(nothing).
/* dignoise */
diagnosis(nothing).
