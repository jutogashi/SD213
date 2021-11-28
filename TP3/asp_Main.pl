/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2020                       */
/* Symbolic Natural Language Processing                          */
/*            http://teaching.dessalles.fr/CANLP                 */
/*---------------------------------------------------------------*/


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% minimal implementation of aspect processing  %
	% Main file                                    %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- assert(language(english)).
% :- assert(language(french)).
	
:- consult('asp_Grammar.pl').	% syntactic rules
:- consult('asp_Merge.pl').	% semantic merge
:- consult('asp_Util.pl').	% contains:  get_line str2wlist setTraceLevel wt

:-	(language(french), consult('asp_Lexique.pl'), consult('asp_Phrases.pl'), !); 
	(language(english), consult('asp_Lexicon.pl'), consult('asp_Sentences.pl')).


:- setTraceLevel(2).	% controls the detail of tracing comments


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% various ways of running the programme
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go :-
	go(2).

go(TraceLevel) :-
	setTraceLevel(TraceLevel),
	test1(FS),
	nl, write('== Ok. '), writeFS(0,FS), !,
	go(TraceLevel).
go(_) :-
	write('Ok').

test1(FS) :-
	nl, nl, write('Sentence --->  '),
	get_line(Sentence),
	process_phrase(s, Sentence, FS).

test :-	% similar to 'go', but gives all solutions
	nl, nl, write('Sentence --->  '),
	get_line(Sentence),
	findall(FS, (process_phrase(s, Sentence, FS), nl, write('== Ok. '), writeFS(0,FS), nl), _FSL).
	%member(FSi, FSL),
	%nl, write('== Ok. '), writeFS(0,FSi),
	%fail.
%test.


test(PhraseType) :-		% to test phrases instead of complete sentences
	nl, writef('Phrase of type %q --->  ', [PhraseType]),
	get_line(Phrase),
	process_phrase(PhraseType, Phrase,FS),
	nl, write('== Ok. '), writeFS(0,FS).
	
process_phrase(PhraseType, Ph, FS) :-
	% calls the DCG grammar	(with PhraseType = s for a sentence)
	R =.. [PhraseType, FS, Ph, []],
	R.
%process_phrase(s, [_,_|_],_) :-
%	nl,write('Réessayer... '), nl, fail.


tests :-
	% runs a series of test on examples taken from a file
	example(_Correct, ExStr, _Comment),
	% writef('\n\n>>>>>>>  %d - %s - %d', [Correct, ExStr, Comment]), nl,
	% format('\n\n>>>>>>>  ~k - ~s - ~k', [Correct, ExStr, Comment]), nl,
	format('\n\n>>>>>>>  ~s', [ExStr]), nl,
	str2wlist(ExStr, Example),
	setof(SFS, (process_phrase(s, Example, FS1), swriteFS(SFS, FS1)), FSl),
	member(FStr, FSl),
	%writef('\nCorrect! - %s - %q', [ ExStr, Comment]),
	nl, wt(0, '===> '),
	wt(0, FStr), nl,
	not(stop(2)),
	!.
tests.

tests0 :-
	setTraceLevel(0),
	tests,
	setTraceLevel(2).

tests00 :-
	tell('Trace0.txt'),
	tests0,
	told,
	write('File "Trace0.txt" created'), nl.

