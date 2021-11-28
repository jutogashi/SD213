/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2020                       */
/* Symbolic Natural Language Processing                          */
/*            http://teaching.dessalles.fr/CANLP                 */
/*---------------------------------------------------------------*/


% implementation minimale du modele temporel


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Syntax
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
	s =  Mary will drink water during one minute
	dp = Mary
	tp = will drink water during one minute
	t =  will
	vp = drink water
	pp = during one minute
*/

s(FS) --> ip(FS), {wt(3,s0) }.	% no time complement
s(FS) --> ip(HFS), l(sep,_), pp(CFS), { merge(s, HFS, CFS, FS), wt(3,s1) }.	% time complement
ip(FS) --> dp(CFS), tp(HFS), { merge(s, HFS, CFS, FS), wt(3,ip) }.
tp(FS) --> l(t,HFS), vpt(CFS), { merge(ip, HFS, CFS, FS), wt(3,tp) }.	% temps
vpt(FS) --> vp(CFS), pp(HFS), { merge(vpt, HFS, CFS, FS), wt(3,vpt1) }.	% time complement 
vpt(FS) --> vp(FS), {wt(3,vpt0)}.	% no time complement
vp(FS) --> l(v,HFS), dp(CFS), {  merge(vp, HFS, CFS, FS), wt(3,vp1) }.	% internal complement 
vp(FS) --> l(vp,FS), {wt(3,vp00) }.	% no complement 
pp(FS) --> l(p,HFS), dp(CFS), { merge(pp, HFS, CFS, FS), wt(3,pp) }.
dp(FS) --> l(dp,FS), {wt(3,dp1)}.	% nom propre
dp(FS) --> l(d,HFS), l(n,CFS), { merge(dp, HFS, CFS, FS), wt(3,dp0) }.	% nom déterminé
dp(FS) --> l(n,CFS), { lexicon(some, HFS), merge(dp, HFS, CFS, FS), wt(3,dp0) }.	% nom déterminé
l(S, FS) --> [Word], {lexicon(Word, HFS), checkF(synt:S, HFS, _), merge(S, HFS, _, FS), wt(3,Word) }.

