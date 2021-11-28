/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2020                       */
/* Symbolic Natural Language Processing                          */
/*            http://teaching.dessalles.fr/CANLP                 */
/*---------------------------------------------------------------*/




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lexical entries : lexicon(<word>, <syntactic and semantic feature structure>)
% Feature structures are unterminated lists: [ Feature1:Value1, Feature2:Value2 | _ ]

lexicon(in, [synt:p, vwp:f, im:op(simult) |_]).
lexicon(during, [synt:p, vwp:g, im:op(simult) |_]).
lexicon('at',[synt:p |_]).

lexicon(',',[synt:sep |_]).

lexicon('Mary', [synt:dp, im:mary|_]).
lexicon('Peter', [synt:dp, im:peter|_]).
lexicon('she', [synt:dp, im:she|_]).
lexicon('he', [synt:dp, im:he|_]).
lexicon(cafeteria,[synt:n, im:cafeteria|_]).
lexicon('cake',[synt:n, im:cake|_]).
lexicon('water',[synt:n, vwp:g, im:water|_]).
lexicon('glass_of_wine',[synt:n, vwp:f, im:glass_of_wine|_]).
lexicon('car',[synt:n, im:car|_]).
lexicon('wine',[synt:n, im:wine|_]).

lexicon(year, [synt:n, det:u, im:year, dur:7.5 |_]).
lexicon(hour, [synt:n, det:u, im:hour, dur:3.6 |_]).
lexicon(minute, [synt:n, det:u, im:minute, dur:1.8 |_]).
lexicon(minutes, [synt:n, det:u, im:minute, dur:2 |_]).
lexicon(second, [synt:n, det:u, im:second, dur:0 |_]).
lexicon(show, [synt:n, det:d, im:show, dur:3.8 |_]).
lexicon(2020, [synt:dp, det:d, im:'2020', dur:7.5 |_]).
lexicon(2010, [synt:dp, det:d, im:'2010', dur:7.5 |_]).


lexicon(like, [synt:v, vwp:g, im:like, dur:nil(1)|_]).

lexicon(eat, [synt:vp, vwp:f, im:eat_meal, dur:3.5 |_FS]).
lexicon(eat, [synt:v, vwp:f, im:ingest, dur:1.4 |_FS]).
lexicon(eat, [synt:v, vwp:g, im:eat_from, dur:1 |_FS]).
lexicon(drink, [synt:v, vwp:f, im:ingest, dur:0.8 |_FS]).
lexicon(drink, [synt:v, vwp:g, im:drink_some, dur:0.8 |_FS]).

lexicon(snore, [synt:vp, vwp:g, im:snore, dur:1|_]).
lexicon(drive, [synt:vp, vwp:g, im:drive, dur:2.6|_]).

lexicon('_PP', [synt:t, vwp:f, det:_, im:past |_]).
lexicon('_PRET', [synt:t, im:past |_]).
lexicon('_PRES', [synt:t, vwp:g |_]).
lexicon('_FUT', [synt:t, vwp:f, im:future |_]).
lexicon('will', [synt:t, vwp:f, im:future |_]).

lexicon(a, [synt:d|FS]) :-
	%FS = [det:d, im:unCertain, occ:sing |_] ;
	FS = [det:u, im:'1', occ:sing |_].	% quantity
lexicon(one, [synt:d|FS]) :-
	FS = [im:'1', occ:sing |_].	% quantity
lexicon(ten, [synt:d, vwp:f, im:'10', occ:mult |_]).
lexicon(the, [synt:d |FS]) :-
	FS = [im:this, occ:sing |_] .
	%FS = [det:u, im:thisKindOf, occ:mult |_].
lexicon(this, FS) :- lexicon(the, FS).
lexicon(some, [synt:d, vwp:g |_]).	 


lexicon(circle, [synt:n, det:d, im:circle|_]).
lexicon(draw, [synt:vp, vwp:g, im:draw_activity, dur:2.5 |_FS]).
lexicon(draw, [synt:v, im:circle, dur:1 |_FS]).
lexicon(sneeze, [synt:vp, vwp:g, im:sneeze, dur:0.1 |_FS]).
lexicon(sleep, [synt:vp, vwp:g, im:sleep, dur:4.5 |_FS]).
lexicon(die, [synt:v, vwp:g, im:die |_FS]).
lexicon(house, [synt:n, det:d, im:house|_]).
	
rephrase(future, 'in the future') :- !.
rephrase(past, 'in the past') :- !.
rephrase(sliced, 'at some moment in') :- !.
rephrase(cover, 'with a duration of') :- !.
rephrase(repeat, repeated) :- !.
rephrase(X,X).
