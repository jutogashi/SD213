%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JL Dessalles -         2014 - www.dessalles.fr                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% implementation minimale du modele temporel




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lexical entries : lexicon(<word>, <syntactic and semantic feature structure>)
% Feature structures are unterminated lists: [ Feature1:Value1, Feature2:Value2 | _ ]

lexicon(en, [synt:p, vwp:f, im:op(simult) |_]).
lexicon(pendant, [synt:p, vwp:g, im:op(simult) |_]).
lexicon(dans, [synt:p, det:d, vwp:f, dur:nil, im:op(after_now) |_]).
lexicon('à',[synt:p |_]).

lexicon(',',[synt:sep |_]).

lexicon('Marie', [synt:dp, im:marie|_]).
lexicon('Pierre', [synt:dp, im:pierre|_]).
lexicon('elle', [synt:dp, im:elle|_]).
lexicon('il', [synt:dp, im:il|_]).
lexicon(cantine,[synt:n, im:cantine|_]).
lexicon('gâteau',[synt:n, im:gateau|_]).
lexicon('voiture',[synt:n, im:voiture|_]).

lexicon(an, [synt:n, det:u, im:anDuree, dur:7.5 |_]).
lexicon(heure, [synt:n, det:u, im:heureDuree, dur:3.6 |_]).
lexicon(minute, [synt:n, det:u, im:minuteDuree, dur:1.8 |_]).
lexicon(minutes, [synt:n, det:u, im:minuteDuree, dur:2 |_]).
lexicon('minute-là', [synt:n, det:d, im:minute, dur:1.81, occ:sing |_]).
lexicon(seconde, [synt:n, det:u, im:secondeDuree, dur:0 |_]).
lexicon(spectacle, [synt:n, det:d, im:spectacle, dur:3.8 |_]).
lexicon(2010, [synt:dp, det:d, im:'2010', dur:7.5 |_]).


lexicon(aime, [synt:v, vwp:g, im:aimer|_]).

lexicon(mange, [synt:vp, vwp:f, im:manger_repas, dur:3.5 |_FS]).
lexicon(mange, [synt:vp, vwp:f, im:grignoter, dur:0.7 |_FS]).
lexicon(mange, [synt:v, vwp:f, im:ingérer, dur:1.4 |_FS]).
lexicon(mange, [synt:v, vwp:g, im:manger_de, occ:mult, dur:2 |_FS]).

lexicon(ronfle, [synt:vp, vwp:g, im:ronfler, dur:1|_]).
lexicon(conduire, [synt:v, vwp:g, im:conduire|_]).



lexicon('_PP', [synt:t, vwp:f, det:_, im:past |_]).
lexicon('_IMP', [synt:t, vwp:g, im:past |_]).
lexicon('_PR', [synt:t, vwp:g |_]).
lexicon('_FUT', [synt:t, vwp:f, im:fut |_]).

lexicon(un, [synt:d|FS]) :-
	%FS = [det:d, im:unCertain, occ:sing |_] ;
	FS = [det:_, im:'1', occ:sing |_].	% quantity
lexicon(une, FS) :- lexicon(un, FS).
lexicon(dix, [synt:d, vwp:f, im:'10', occ:mult |_]).	
%lexicon(dix, [synt:d, det:u, im:'10', occ:sing |_]).	% yes, singular
lexicon(le, [synt:d |FS]) :-
	FS = [det:_, im:ce, occ:sing |_] .
	%FS = [det:u, im:ceTypeDe, occ:mult |_].
lexicon(la, FS) :- lexicon(le, FS).
lexicon(cette, FS) :- lexicon(le, FS).
lexicon(ce, FS) :- lexicon(le, FS).
lexicon(du, [synt:d, vwp:g |_]).	 


rephrase(future, 'dans le futur') :- !.
rephrase(past, 'dans le passé') :- !.
rephrase(sliced, 'à un moment de') :- !.
rephrase(cover, 'sur la durée de') :- !.
rephrase(repeat, répété) :- !.
rephrase(after, 'après') :- !.
rephrase(X,X).
