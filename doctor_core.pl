/* */
/* Import the conversing.pl file */
:-[conversing].
/* set those predicates so it can change during execution */
:- dynamic pain/1.
:- dynamic mood/1.
:- dynamic have/1.
:- dynamic nothave/1.
:- dynamic diagnosis/1.

/* -----------------------beginnig of questions----------------------- */
ask(0):-	pain_library(P), write('Nice to meet to my friend. What\' s up today? '), ask_pain(P).

/* Recursion of a list. The actual functionality is to iterate the pain list, ask the patients
 * from the least amount of pain to the most severe pain */
ask_pain([PH|PT]):-
    /* select a random opening and question start from conversing.pl */
    opening(OP), 
    question_start(QS), 
    write(OP), write(QS), to_string(PH, PHS), write(PHS),
    write('? y/n/q: '), 
    /* read the input */
    read(HasPain), 
    (
        HasPain==q -> abort;
        /* if the patient opts yes, assert this pain level, assert this as a symptom 
         * that the patient has, so the doctor will not ask it again in ask_symptom(   ), and proceed to ask mood */
        HasPain==y -> assert(pain(PH)), assert(have(PH)), mood_library(M), ask_mood(M); 
        /* if the patient anwsers no for all pain level, comfort the patient and ask again */
        HasPain==n -> assert(nothave(PH)),(PT==[] -> write('Please tell me how much pain you feel. Take time, let\'s start again.'), pain_library(P), ask_pain(P); ask_pain(PT))
    ).

/* Recursion of a list. The actual functionality is to iterate the mood list, ask the patients
 * from the most happy mood to the most sad mood */
ask_mood([MH|MT]):-
   opening(OP),
   question_start(QS), 
   write(OP), write(QS), to_string(MH, MHS), write(MHS),
   write('? y/n/q: '), 
   read(HasMood), 
   (
       HasMood==q -> abort;
       /* if the patient opts yes, assert this mood level, assert this as a symptom 
         * that the patient has, so the doctor will not ask it again in ask_symptom(   ), and proceed to ask symptom */
       HasMood==y -> assert(mood(MH)), assert(have(MH)), ask_symptom(0); 
       /* if the patient anwsers no for all mood level, comfort the patient and ask again */
       HasMood==n -> assert(nothave(MH)), (MT==[] -> write('Please tell me about you mood. Don\'t worry, take your time'), mood_library(M), ask_mood(M);  ask_mood(MT))
   ).

/* begin asking symptoms with [temperature]*/
ask_symptom(0):-
    /* select a gesture using sympathetic(G), then convert it to human readable string
     * using to_string() from conversing.pl */
    sympathetic(G), write('speaking with '), to_string(G, GS), write(GS), write(': '),
    opening(OP), write(OP), 
    validate_and_query_options([temperature]).

ask_symptom(Y):- 
    /* if diagnose(X) completed successfully, which means have at least one diagnose X ,
     * write result and end the system. */
    diagnose(X), write('My dear patient. Your diagnosis is '), write(X), write('. I hope you get better soon.');
    /* otherwise, continue to ask symptoms */
    generate_options(Y,L), validate_and_query_options(L).

generate_options(Y,L):-
    /* select a gesture */
    sympathetic(G), write('speaking with '), to_string(G, GS), write(GS), write(': '), question_start(QS),
    (
        /* if the patients says yes, select related symptoms as L */
        have(Y), write("Ah, I see. "),  write(QS), findnsols(100,X,related(X,Y),L);
        /* if the patients says yes, select random symptoms as L */
        nothave(Y), write("Great. "),  write(QS), findnsols(100,X,random(X),L)
    ).

/* determine which symptom to ask and process the received response */
validate_and_query_options(L):-
    /* to find valid symptoms to ask:
     * find the symptoms the patient has or doesn't have, these two list combined to history
     * then ask one of these symptoms which is not asked before */
	findnsols(100, X, have(X), Havelist), 
    findnsols(100, X, nothave(X), Nothavelist), 
    append(Havelist, Nothavelist, History), 
    list_to_set(L,S), 
    list_to_set(History,H), 
    subtract(S,H,Valid), 
    /* to find all unasked symptoms */
    fever(A), cold(B), injury(C), depression(D), food_poisoning(E), append(A,B,AB), append(AB,C,ABC), append(ABC,D,ABCD), append(ABCD,E,ABCDE), 
    list_to_set(ABCDE, All),
    subtract(All,H, Unasked), 
    (
        /* if all symptoms are asked and no diseases are diagnosed, write the result and end system*/
        Unasked==[]-> write("alright? I think you are well. Relax, my dear patient. However, if you do feel sick, please approach other doctor. Have a good day! ");
        /* otherwise, keep asking symptoms. If there is no valid related symptoms, select one form Unasked*/
        (   Valid ==[]->  member(X,Unasked), to_string(X, XS), write(XS), write('? y/n/q: ');
            member(X,Valid), to_string(X, XS), write(XS), write('? y/n/q: ')
        ),
        /* read the response and process the result */
        read(Have), 
        (
            Have==q -> abort;
            Have==y -> assert(have(X));
            Have==n -> assert(nothave(X))
        ), ask_symptom(X) 
    ).
   

/* define related symptoms: they are related if they are symptoms of a same diagnose*/
related(X,Y):- 
	fever(L),member(X,L),member(Y,L);
	cold(L),member(X,L),member(Y,L);
	injury(L),member(X,L),member(Y,L);
	depression(L),member(X,L),member(Y,L);
	food_poisoning(L),member(X,L),member(Y,L).

/* random select symptoms topic */
random(X):-
	fever(A), cold(B), injury(C), depression(D), food_poisoning(E), append(A,B,AB), append(AB,C,ABC), append(ABC,D,ABCD), append(ABCD,E,ABCDE), random_member(X,ABCDE).



/* define sympathetic doctor */
sympathetic(G):-
    (
        /* no pain and calm mood, set gesture to normal */
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
    
/* count number of symptoms matches for each diagnosis, if one reaches 4, diagnosis is set to that disease */
diagnose(Y):-
    fever(F), cold(C), injury(I), depression(D), food_poisoning(FP),
    /* the symptoms set for each disease */
    list_to_set(F, FS), list_to_set(C, CS), list_to_set(I, IS), list_to_set(D, DS), list_to_set(FP, FPS),
    /* the set of symptoms that the patient has */
    findall(X, have(X), L), list_to_set(L, LS),
    (
        intersection(FS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(fever)));
        intersection(CS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(cold)));
        intersection(IS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(injury)));
        intersection(DS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(depression)));
        intersection(FPS, LS, Result), length(Result, Num), (Num @> 3 -> assert(diagnosis(food_poisoning)))
    ),
    /* the set of diagnose given to the patient */
    findall(X,diagnosis(X),Diag), diagnosis_library(DL),
    intersection(Diag, DL, Y).

/* -----------------------predicates defined----------------------- */
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
