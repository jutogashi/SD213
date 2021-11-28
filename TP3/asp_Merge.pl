/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2020                       */
/* Symbolic Natural Language Processing                          */
/*            http://teaching.dessalles.fr/CANLP                 */
/*---------------------------------------------------------------*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merging of structures 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-----------------------------------------

%------------%
% MAIN MERGE %
%------------%
% When Syntax merges two phrases, it calls 'merge' to merge the corresponding semantic structures
merge(SCat, FS1, FS2, FS) :-
				wt(5,'\nmerging:      '), writeFS(5,FS1), writeFS(5,FS2), stop(6),
	merge_sem(FS1, FS2, FSa),	% Semantic merge: feature structure merge
	checkF(synt:_, FSa, FSb),	% lower syntactic categories are erased
	execute([synt:SCat | FSb], FSc),	% action of eventual operator
	rescue(FSc, FS).			% Rescue through predication and-or repetition


%-----------------------%
% Procedural semantics  %
%-----------------------%
execute(FS, FS2) :-		% execution of operators
				wt(6,'\nFirst pass:   '), writeFS(6,FS),
	checkFActual(im:Im, FS, FS1),
	Im =.. [O, I],
	member(O, [simult]),
	!,
				wt(6,'\nexecution:    '), writeFS(6, FS), stop(6),
				wt(4,('\nperf.', O, on)), writeFS(4,[im:I | FS1]), stop(6),
	B =.. [O, [im:I | FS1], FS2],
	B.	% perform operation on FS1 --> FS2
execute(FS, FS).	% by default, 'execute' does nothing
	
%-----------------------%
% feature structure merge
%-----------------------%
merge_sem([F|_], FS, FS) :- 
	var(F),
	% No feature left
	!.		
merge_sem([ F:V1 | R1 ], FS2, [F:V | NFS]) :-
	select(F:V2, FS2, R2),
	!,
	% V1 and V2 have to be merged as F-feature values
	merge_value(F, V1, V2, V), % calls specialized Merges
	merge_sem(R1, R2, NFS).
merge_sem(FS1, FS2, _) :-
	wt(2,'\nEchec unif:   '), writeFS(2,FS1), writeFS(2,FS2), fail.

checkF(F:V, FS1, R) :-	% checks whether feature is possibly present
	select(F:V1, FS1, R),
	!, V = V1.

checkFActual(F:V, FS1, R) :-	% checks that feature is actually present
	select(F:V1, FS1, R), 
	!, ground(V1), V = V1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merging of features. merge_value(F, V1, V2) : V1 and V2 are merged as features of type F
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge_value(_, V, V, V) :- !.		% perfect merge - Nothing more to do
merge_value(synt, H, _, H) :- !.	% syntactic merge: head wins 
merge_value(im, Im1, Im2, Im) :-	% perceptive merge
	!,
	merge_picture(Im1, Im2, Im).
merge_value(dur, D1, D2, D) :-
	ground((D1, D2)),	% both durations are instantiated
	dcompare(D1, D2, D),	% checks whether both durations are compatible
	!.
merge_value(F, V1, V2, _) :-
	wt(3,('\nUnification problem', F:V1, F:V2)),
	fail.
	
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simult(FS, [im:Im1 | FS3]) :-	
	%%%%% unification:	dur:D --> cover + occ:_
	checkF(dur:D, FS, _), number(D),	
	checkF(occ:_, FS, FS2),	% forget about multiplicity
	checkF(im:Im, FS2, FS3),
	merge_picture('cover', Im, Im1), 
		wt(5, ('\nsimult through mere unification')), writeFS(5,FS).
simult(FS, [im:Im1, dur:max(D), vwp:f, occ:sing | FS5]) :-
	%%%%% slice:	det:d + dur:D --> dur:max(D) + vwp:f + occ:sing + slice
		wt(5,'\nRescuing attempt by slicing'), writeFS(5,FS),
	checkFActual(det:d, FS, _),	% only already determined periods can be sliced
	% another constraint might be missing: that Im be derivable
	checkF(vwp:_, FS, FS1),		% viewpoint is erased
	checkF(occ:_, FS1, FS3),	% multiplicity is erased
	checkF(im:Im, FS3, FS4),
	checkF(dur:D, FS4, FS5), number(D),	% duration is replaced by max(D)
	merge_picture('sliced', Im, Im1), 
		wt(5, ('\nRescued', Im, 'by slicing')), writeFS(5,FS).
simult(FS, [im:Im1, det:d | FS3]) :-
	%%%%% inchoactivity:	vwp:f + det:u + dur:D --> det:d + dur:_ + after
	% One should consider that the resulting d needs grounding - hence the use of present tense
	checkFActual(vwp:f, FS, _),
	checkFActual(det:u, FS, FS1),
	checkF(dur:D, FS1, FS2), number(D),	% erasing duration
	checkF(im:Im, FS2, FS3),
	rephrase(after, After),
	merge_picture(After, Im, Im1), 
		wt(5, ('\nRescued', Im, 'through inchoativity')), writeFS(5,FS).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rescue operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rescue(FS, FS).			% no rescue by default
rescue(FS1, FS3) :-		% rescue by repetition
	repeat(FS1, FS2),
	rescue(FS2, FS3).
rescue(FS1, FS3) :-		% rescue by predication
	predicate(FS1, FS2),
	rescue(FS2, FS3).

	
repeat(FS, [im:Im1, dur:min(D1), vwp:g, occ:mult | FS4]) :-
	%%%%% repeat:	synt:vp + occ:_ + vwp:f + dur:D --> occ:mult + vwp:g + dur:min(D+0.7) + det:_
	checkFActual(synt:S, FS, _), member(S,[vp]),	% repeat only actions
		wt(5,'\nRescuing attempt by repeating'), writeFS(5,FS),
	checkF(occ:O, FS, FS1), var(O),	% singular or multiple events cannot be repeated
	checkF(vwp:f, FS1, FS2),
	checkF(im:Im, FS2, FS3),
	checkF(dur:D, FS3, FS4), number(D), D1 is D + 0.7,
	checkF(det:_, FS4, _), 	% erasing determination
	checkF(repetition:Im, FS4, _),
	merge_picture('repeat', Im, Im1), 
		wt(4, ('\nRescued       ', Im, 'by repeating')), writeFS(4,FS).

predicate(FS, [vwp:f, im:Im1, dur:nil(D), occ:sing | FS5]) :-
	%%%%% predication:	synt:<pred_phrase> + occ:sing + dur:D --> dur:nil(D) + vwp:f + occ:sing + pred
	checkFActual(synt:S, FS, _), member(S,[vp, vpt, dp, vpt]),
		wt(5,'\nResc. by pred:'), writeFS(5,FS),
	checkF(vwp:_, FS, FS1),		% erasing viewpoint
	checkF(occ:_, FS1, FS3),	% erasing multiplicity
	checkF(dur:D, FS3, FS4),
	checkF(im:Im, FS4, FS5),
	concrete(Im),	% conceptual predication
	checkF(pred:Im, FS5, _),	% prevents from multiple predications
	merge_picture('!', Im, Im1), 
		wt(4, ('\nRescued', Im, 'by predicating')), writeFS(4,FS).

concrete(Im) :-	% This can be implemented as a restriction on what can be predicated upon
	ground(Im), 
	Im \== ''.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interface with perception
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_picture(Im1, Im2, Im) :-
	reify(Im1, Im1a),	% suppresses variables for display
	reify(Im2, Im2a),	% suppresses variables for display
	Im1a =.. [O | A], 
	Im =.. [O, Im2a | A	].	% mere embedding of image identifiers
	
	
reify(Im, '') :-
	% suppresses variables for display
	var(Im), !.
reify(op(O), O) :- 	!.
reify(Im, Im).	


%%%%%%%%%%%%%%%%%%%%%%%%%%
% duration compatibility %
%%%%%%%%%%%%%%%%%%%%%%%%%%

dcompare(D1, max(D2), D1) :- 	smaller(D1, D2), !.
dcompare(max(D1), D2, D2) :- 	smaller(D2, D1), !.
dcompare(D1, min(D2), D1) :- 	smaller(D2, D1), !.
dcompare(min(D1), D2, D2) :- 	smaller(D1, D2), !.
dcompare(min(D1), min(D2), min(D2)) :-	smaller(D1, D2), !.
dcompare(min(D1), min(_D2), min(D1)) :- !.
dcompare(max(D1), max(D2), max(D1)) :-	smaller(D1, D2), !.
dcompare(max(_D1), max(D2), max(D2)) :- !.
dcompare(nil(D1), max(D2), max(D2)) :-	smaller(D1, D2), !.
dcompare(max(D1), nil(D2), max(D1)) :-	smaller(D2, D1), !.
dcompare(D1, D2, D2) :- 	% duration merge
	number(D1), number(D2), abs(D1-D2, G), 
	G < 1.1, !.	% less than one order of magnitude gap

smaller(_, min(_)).
smaller(max(_), _).
smaller(D1, D2) :- 	
	number(D1), number(D2), D1 =< D2.
smaller(min(D1), D2) :-	smaller(D1, D2).
smaller(D1, max(D2)) :-	smaller(D1, D2).
smaller(D1, max(D2)) :-	smaller(D1, D2).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display feature structures 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeFS(TrLevel, FS) :-
	swriteFS(S, FS), 
	wt(TrLevel, S),
	fail.	% forces backtrack because swriteSF changes FS
writeFS(_, _).
	
swriteFS(Str, FS) :-
	% Attention: FS gets modified
	checkF(synt:S, FS, R1), checkF(vwp:V, R1, R2), checkF(det:A, R2, R3),
	%sort(FS, FSSorted),
	checkF(pred:_, R3, R4),
	length(R4,_FSLength),	% destroys the unterminated tail
	!,
	checkF(im:Im, R4, R5), 
	% swritef(Str1, "%d.%d.%d\t\t%t", [S,V,A,R5]),
	format(string(Str1), "~k.~k.~k\t\t~p", [S,V,A,R5]),
	paraphrase(Im, StrIm),
	string_concat(Str1, '\n --->\t', Str2),
	string_concat(Str2, StrIm, Str).

paraphrase(Im, StrIm) :-
	Im =.. [V, S, C],
	rephrase(V, V1),
	paraphrase(S, S1),
	paraphrase(C, C1),
	% swritef(StrIm, "%w %w %w", [S1,V1,C1]).	
	format(string(StrIm), "~w ~w ~w", [S1,V1,C1]).	
paraphrase(Im, StrIm) :-
	Im =.. ['!', C], !,
	paraphrase(C, C1),
	% swritef(StrIm, "(%w)!", [C1]).	
	format(string(StrIm), "(~w)!", [C1]).	
paraphrase(Im, StrIm) :-
	Im =.. [V, C],
	paraphrase(C, C1),
	% swritef(StrIm, "%w %w", [V,C1]).
	format(string(StrIm), "~w ~w", [V,C1]).
paraphrase(N, StrIm) :-
	atom(N),
	% swritef(StrIm, "%w", [N]).
	format(string(StrIm), "~w", [N]).
	
	