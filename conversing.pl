

/* convert the libraries to fancy natural languages */
convert(unbearable_pain, 'unbearable pain ').
convert(lot_of_pain, 'lot of pain ').
convert(manageable_pain, 'manageable pain ').
convert(mild_pain, 'mild pain ').
convert(no_pain, 'no pain at all ').

convert(calm, 'calm ').
convert(angry, 'angry ').
convert(weepy, 'weepy and want to cry ').
convert(stressed, 'stressed ').
convert(depressed, 'depressed ').


convert(temperature, 'you have unusual body temperature ').
convert(sweat, 'sweaty ').
convert(ache, 'ache ').
convert(sneeze, 'sneezing ').
convert(cough, 'coughing ').
convert(blood, 'you have blood loss ').
convert(bonebreak, 'you have a bone break ').
convert(inactive, 'inactive ').
convert(insomnia, 'you have insomnia recently ').
convert(rash, 'you have rash on your skin ').
convert(stomachache, 'you have stomachache ').
convert(nausea, 'you have nausea ').
convert(high_heart_rate, 'have high heart rate ').

convert(fever, 'fever').
convert(cold, 'cold').
convert(injury, 'injury').
convert(depression, 'depression').
convert(food_poisoning, 'food poisoning').

convert(look_concerned, 'concerned look').
convert(mellow_voice, 'mellow voice').
convert(light_touch, 'light touch on youur arm').
convert(faint_smile, 'faint smile').
convert(eye_contact, 'sympathetic eye contact').
convert(greet, 'greetings').
convert(look_composed, 'composed look').
convert(look_attentive, 'attentive look').
convert(calming_voice, 'calming voice').
convert(soft_touch, 'soft touch on your hand').
convert(broad_smile, 'broad smile').
convert(joke, 'a light joke').
convert(beaming_voice, 'beaming voice').
convert(energetic_greeting, 'energetic greeting').
convert(happy_smile, 'happy smile').

/* greeting part of a interrogative sentence */
openings('Well, ').
openings('Relax. My friend, ').
openings('My dear friend, ').
openings('No hurry, dear,  ').
openings('No worries, and ').
openings('I will help you, and ').
openings('Take your time, and ').
openings('Take your time, just talk to me, ').
/* get a random one from openings */
opening(OP):-
    findall(A, openings(A), OpeningsList),
    random_member(OP, OpeningsList).

/* asking part of a interrogative sentence */
question_starts('are you feeling ').
question_starts('do you feel ').
question_start(QS):-
    findall(S, question_starts(S), Question_startsList),
    random_member(QS, Question_startsList).

/* map an atom to human readable string */
to_string(A, S):-
    convert(A, S).
   