/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2020                       */
/* Symbolic Natural Language Processing                          */
/*            http://teaching.dessalles.fr/CANLP                 */
/*---------------------------------------------------------------*/



% ----------------------
% Phrase structure rules
% ----------------------

% Phrases have three arguments:
% xp(FXP, PXP, TXP)
% FXP is the feature structure, e.g. [gloss:like,  num:plur,pers:3,subj:dp(_),  cpl:[dp(_)]]
%   (gloss indicates a string used for output and trace)
% PXP is the predicative structure, e.g. like(_,_)
% TXP is the tree structure, used for display, e.g. v(like)


% att is used to access to slots in feature structures
% link  implements semantic linking


% sentence
s(FVP,PS,TS) --> dp(FDP,PDP,TDP), vp(FVP,PVP,TVP), {  % determiner phrase + verb phrase
    att(FVP, subj, dp(FDP)),
    att(FDP, num, Num),   % checking for number agreement 
    att(FVP, num, Num),   % between subject and verb
    link(1, PVP, PDP, PS),
    TS  = s(TDP,TVP) }.

s(FVP,PVP,TS) --> [it], vp(FVP,PVP,TVP), {  % impersonal verb  (it rains)
    att(FVP, subj, [it]),
    TS  = s(TVP) }.


% verb phrase
vp(FV,PV,TVP) --> v(FV,PV,TV), {        % non transitive verb, eg. 'sleep'
    att(FV, cpl, []),
    TVP = vp(TV) }.

vp(FV,PVP,TVP) --> v(FV,PV,TV), dp(FDP,PDP,TDP), { % transitive verb, eg. 'like'
    att(FV, cpl, [dp(FDP)]),
    link(2, PV,PDP,PVP),
    TVP  = vp(TV,TDP) }.

vp(FV,PVP,TVP) --> v(FV,PV,TV), pp(FPP,PPP,TPP), { % transitive verb, indirect object: 'dream'
    att(FV, cpl, [pp(P)]),
    att(FPP, gloss, P),
    link(2, PV,PPP,PVP),
    TVP  = vp(TV,TPP) }.

vp(FV,PVP,TVP) --> v(FV,PV,TV), pp(FPP,PPP,TPP), pp(FPP2, PPP2, TPP2), { % ditransitive  verb, indirect objects: 'talk'
    att(FV, cpl, [pp(P), pp(P2)]),
    att(FPP, gloss, P),
    att(FPP2, gloss, P2),
    link(2, PV,PPP,PV1),
    link(3, PV1,PPP2,PVP),
    TVP  = vp(TV,TPP, TPP2) }.

vp(FV,PVP,TVP) --> v(FV,PV,TV), dp(FDP,PDP,TDP), dp(FDP2,PDP2,TDP2), { % ditransitive verb, eg. 'give'
    att(FV, cpl, [dp(FDP),dp(FDP2)]),
    link(2, PV,PDP,PV1),
    link(3, PV1,PDP2,PVP),
    TVP  = vp(TV, TDP, TDP2) }.

vp(FV,PVP,TVP) --> v(FV,PV,TV), cp(FCP,PCP,TCP), { % verb + cp: eg. 'dreams', 'believes'...
    att(FV, cpl, [cp(P)]),
    att(FCP, gloss, P),
    link(2, PV,PCP,PVP),
    TVP  = vp(TV, TCP) }.

vp(FV,PA,TVP) --> v(FV,_PV,TV), adj(_FADJ,PA,TADJ), { % verb + adj: eg. 'is', 'looks', 'seems'...
    att(FV, cpl, [adj(_)]),
    TVP  = vp(TV, TADJ) }.


% noun phrases
dp(FDP,PPN,TDP) --> pn(FDP,PPN,TPN), {  % proper noun
    TDP = dp(TPN) }.

dp(FN,PNP,TDP) --> det(FDET,TDET), np(FN,PNP,TN), { % 'the girl'
    att(FDET, num, Num), 
    att(FN, num, Num),
    TDP = dp(TDET,TN) }.

dp(FN,PNP,TDP) --> np(FN,PNP,TN), { % 'girls'
    att(FN, num, plur), 
    TDP = dp(TN) }.


np(FN,PN,TNP) --> n(FN,PN,TN), {  % 'girl'
    TNP = np(TN) }.

np(FNP,PNP1,TNP) --> adj(_FADJ,PA,TADJ), np(FNP,PNP1,TNP1), { % 'nice girl'
    link(1,PNP1,PA,_PNP),
    TNP = np(TADJ, TNP1) }.

np(FN,PNP,TNP) --> n(FN,PN,TN), pp(_FPP,PPP,TPP), { % 'room of the girl'
    link(2,PN,PPP,PNP),
    TNP = np(TN, TPP) }.


% prepositional phrases
pp(FP, PDP,TPP) --> p(FP,TP), dp(_FDP,PDP,TDP), {
    TPP = pp(TP, TDP) }.


% subordinate clauses
cp(FC, PS,TCP) --> c(FC, TC), s(_FS, PS,TS), {
    TCP = cp(TC, TS) }.


% -------
% Lexicon
% -------
det([gloss:the, num:sing], det(the)) --> [the].
det([gloss:the, num:plur], det(the)) --> [the].
det([gloss:a,   num:sing], det(a))   --> [a].
det([gloss:all, num:plur], det(all)) --> [all].

n([gloss:child, num:sing], child(_), n(child)) --> [child].
n([gloss:child, num:plur], child(_), n(child)) --> [children].
n([gloss:daughter, num:sing], daughter(_, _), n(daughter)) --> [daughter].
n([gloss:game,  num:sing], game(_),  n(game))  --> [game].
n([gloss:game,  num:plur], game(_),  n(game))  --> [games].
n([gloss:girl,  num:sing], girl(_),  n(girl))  --> [girl].
n([gloss:girl,  num:plur], girl(_),  n(girl))  --> [girls].
n([gloss:boy,   num:sing], boy(_),   n(boy))   --> [boy].
n([gloss:boy,   num:plur], boy(_),   n(boy))   --> [boys].
n([gloss:room,  num:sing], room(_),  n(room))  --> [room].
n([gloss:house, num:plur], house(_), n(house)) --> [house].
n([gloss:hall, num:plur],  hall(_),  n(hall))  --> [hall].
n([gloss:garden, num:plur],garden(_),n(garden)) --> [garden].

% nouns used in the Pokemon example
n([gloss:type,   num:sing], type(_, _),   n(type))   --> [type].
n([gloss:pokemon,   num:sing], pokemon(_),   n(pokemon))   --> [pokemon].

n([gloss:bug,   num:sing], bug(_),   n(bug))   --> [bug].
n([gloss:dragon,   num:sing], dragon(_),   n(dragon))   --> [dragon].
n([gloss:eletric,   num:sing], eletric(_),   n(eletric))   --> [eletric].
n([gloss:fighting,   num:sing], fighting(_),   n(fighting))   --> [fighting].
n([gloss:fire,   num:sing], fire(_),   n(fire))   --> [fire].
n([gloss:flying,   num:sing], flying(_),   n(flying))   --> [flying].
n([gloss:ghost,   num:sing], ghost(_),   n(ghost))   --> [ghost].
n([gloss:grass,   num:sing], grass(_),   n(grass))   --> [grass].
n([gloss:ground,   num:sing], ground(_),   n(ground))   --> [ground].
n([gloss:ice,   num:sing], ice(_),   n(ice))   --> [ice].
n([gloss:normal,   num:sing], normal(_),   n(normal))   --> [normal].
n([gloss:poison,   num:sing], poison(_),   n(poison))   --> [poison].
n([gloss:psychic,   num:sing], psychic(_),   n(psychic))   --> [psychic].
n([gloss:rock,   num:sing], rock(_),   n(rock))   --> [rock].
n([gloss:water,   num:sing], water(_),   n(water))   --> [water].

% proper nouns
pn([gloss:john, num:sing], john, pn(john)) --> ['John'].
pn([gloss:pat,  num:sing], pat, pn(pat))   --> ['Pat'].
pn([gloss:mary, num:sing], mary, pn(mary)) --> ['Mary'].
pn([gloss:ann,  num:sing], ann, pn(ann))   --> ['Ann'].

% proper nouns used in the Pokemon example


pn([gloss:ash,  num:sing], ash, pn(ash))   --> ['Ash'].

pn([gloss: bulbasaur,  num:sing], bulbasaur, pn(bulbasaur))   --> ['Bulbasaur'].
pn([gloss: ivysaur,  num:sing], ivysaur, pn(ivysaur))   --> ['Ivysaur'].
pn([gloss: venusaur,  num:sing], venusaur, pn(venusaur))   --> ['Venusaur'].
pn([gloss: charmander,  num:sing], charmander, pn(charmander))   --> ['Charmander'].
pn([gloss: charmeleon,  num:sing], charmeleon, pn(charmeleon))   --> ['Charmeleon'].
pn([gloss: charizard,  num:sing], charizard, pn(charizard))   --> ['Charizard'].
pn([gloss: squirtle,  num:sing], squirtle, pn(squirtle))   --> ['Squirtle'].
pn([gloss: wartortle,  num:sing], wartortle, pn(wartortle))   --> ['Wartortle'].
pn([gloss: blastoise,  num:sing], blastoise, pn(blastoise))   --> ['Blastoise'].
pn([gloss: caterpie,  num:sing], caterpie, pn(caterpie))   --> ['Caterpie'].
pn([gloss: metapod,  num:sing], metapod, pn(metapod))   --> ['Metapod'].
pn([gloss: butterfree,  num:sing], butterfree, pn(butterfree))   --> ['Butterfree'].
pn([gloss: weedle,  num:sing], weedle, pn(weedle))   --> ['Weedle'].
pn([gloss: kakuna,  num:sing], kakuna, pn(kakuna))   --> ['Kakuna'].
pn([gloss: beedrill,  num:sing], beedrill, pn(beedrill))   --> ['Beedrill'].
pn([gloss: pidgey,  num:sing], pidgey, pn(pidgey))   --> ['Pidgey'].
pn([gloss: pidgeotto,  num:sing], pidgeotto, pn(pidgeotto))   --> ['Pidgeotto'].
pn([gloss: pidgeot,  num:sing], pidgeot, pn(pidgeot))   --> ['Pidgeot'].
pn([gloss: rattata,  num:sing], rattata, pn(rattata))   --> ['Rattata'].
pn([gloss: raticate,  num:sing], raticate, pn(raticate))   --> ['Raticate'].
pn([gloss: spearow,  num:sing], spearow, pn(spearow))   --> ['Spearow'].
pn([gloss: fearow,  num:sing], fearow, pn(fearow))   --> ['Fearow'].
pn([gloss: ekans,  num:sing], ekans, pn(ekans))   --> ['Ekans'].
pn([gloss: arbok,  num:sing], arbok, pn(arbok))   --> ['Arbok'].
pn([gloss: pikachu,  num:sing], pikachu, pn(pikachu))   --> ['Pikachu'].
pn([gloss: raichu,  num:sing], raichu, pn(raichu))   --> ['Raichu'].
pn([gloss: sandshrew,  num:sing], sandshrew, pn(sandshrew))   --> ['Sandshrew'].
pn([gloss: sandslash,  num:sing], sandslash, pn(sandslash))   --> ['Sandslash'].
pn([gloss: nidoran_f,  num:sing], nidoran_f, pn(nidoran_f))   --> ['Nidoran_f'].
pn([gloss: nidorina,  num:sing], nidorina, pn(nidorina))   --> ['Nidorina'].
pn([gloss: nidoqueen,  num:sing], nidoqueen, pn(nidoqueen))   --> ['Nidoqueen'].
pn([gloss: nidoran_m,  num:sing], nidoran_m, pn(nidoran_m))   --> ['Nidoran_m'].
pn([gloss: nidorino,  num:sing], nidorino, pn(nidorino))   --> ['Nidorino'].
pn([gloss: nidoking,  num:sing], nidoking, pn(nidoking))   --> ['Nidoking'].
pn([gloss: clefairy,  num:sing], clefairy, pn(clefairy))   --> ['Clefairy'].
pn([gloss: clefable,  num:sing], clefable, pn(clefable))   --> ['Clefable'].
pn([gloss: vulpix,  num:sing], vulpix, pn(vulpix))   --> ['Vulpix'].
pn([gloss: ninetales,  num:sing], ninetales, pn(ninetales))   --> ['Ninetales'].
pn([gloss: jigglypuff,  num:sing], jigglypuff, pn(jigglypuff))   --> ['Jigglypuff'].
pn([gloss: wigglytuff,  num:sing], wigglytuff, pn(wigglytuff))   --> ['Wigglytuff'].
pn([gloss: zubat,  num:sing], zubat, pn(zubat))   --> ['Zubat'].
pn([gloss: golbat,  num:sing], golbat, pn(golbat))   --> ['Golbat'].
pn([gloss: oddish,  num:sing], oddish, pn(oddish))   --> ['Oddish'].
pn([gloss: gloom,  num:sing], gloom, pn(gloom))   --> ['Gloom'].
pn([gloss: vileplume,  num:sing], vileplume, pn(vileplume))   --> ['Vileplume'].
pn([gloss: paras,  num:sing], paras, pn(paras))   --> ['Paras'].
pn([gloss: parasect,  num:sing], parasect, pn(parasect))   --> ['Parasect'].
pn([gloss: venonat,  num:sing], venonat, pn(venonat))   --> ['Venonat'].
pn([gloss: venomoth,  num:sing], venomoth, pn(venomoth))   --> ['Venomoth'].
pn([gloss: diglett,  num:sing], diglett, pn(diglett))   --> ['Diglett'].
pn([gloss: dugtrio,  num:sing], dugtrio, pn(dugtrio))   --> ['Dugtrio'].
pn([gloss: meowth,  num:sing], meowth, pn(meowth))   --> ['Meowth'].
pn([gloss: persian,  num:sing], persian, pn(persian))   --> ['Persian'].
pn([gloss: psyduck,  num:sing], psyduck, pn(psyduck))   --> ['Psyduck'].
pn([gloss: golduck,  num:sing], golduck, pn(golduck))   --> ['Golduck'].
pn([gloss: mankey,  num:sing], mankey, pn(mankey))   --> ['Mankey'].
pn([gloss: primeape,  num:sing], primeape, pn(primeape))   --> ['Primeape'].
pn([gloss: growlithe,  num:sing], growlithe, pn(growlithe))   --> ['Growlithe'].
pn([gloss: arcanine,  num:sing], arcanine, pn(arcanine))   --> ['Arcanine'].
pn([gloss: poliwag,  num:sing], poliwag, pn(poliwag))   --> ['Poliwag'].
pn([gloss: poliwhirl,  num:sing], poliwhirl, pn(poliwhirl))   --> ['Poliwhirl'].
pn([gloss: poliwrath,  num:sing], poliwrath, pn(poliwrath))   --> ['Poliwrath'].
pn([gloss: abra,  num:sing], abra, pn(abra))   --> ['Abra'].
pn([gloss: kadabra,  num:sing], kadabra, pn(kadabra))   --> ['Kadabra'].
pn([gloss: alakazam,  num:sing], alakazam, pn(alakazam))   --> ['Alakazam'].
pn([gloss: machop,  num:sing], machop, pn(machop))   --> ['Machop'].
pn([gloss: machoke,  num:sing], machoke, pn(machoke))   --> ['Machoke'].
pn([gloss: machamp,  num:sing], machamp, pn(machamp))   --> ['Machamp'].
pn([gloss: bellsprout,  num:sing], bellsprout, pn(bellsprout))   --> ['Bellsprout'].
pn([gloss: weepinbell,  num:sing], weepinbell, pn(weepinbell))   --> ['Weepinbell'].
pn([gloss: victreebel,  num:sing], victreebel, pn(victreebel))   --> ['Victreebel'].
pn([gloss: tentacool,  num:sing], tentacool, pn(tentacool))   --> ['Tentacool'].
pn([gloss: tentacruel,  num:sing], tentacruel, pn(tentacruel))   --> ['Tentacruel'].
pn([gloss: geodude,  num:sing], geodude, pn(geodude))   --> ['Geodude'].
pn([gloss: graveler,  num:sing], graveler, pn(graveler))   --> ['Graveler'].
pn([gloss: golem,  num:sing], golem, pn(golem))   --> ['Golem'].
pn([gloss: ponyta,  num:sing], ponyta, pn(ponyta))   --> ['Ponyta'].
pn([gloss: rapidash,  num:sing], rapidash, pn(rapidash))   --> ['Rapidash'].
pn([gloss: slowpoke,  num:sing], slowpoke, pn(slowpoke))   --> ['Slowpoke'].
pn([gloss: slowbro,  num:sing], slowbro, pn(slowbro))   --> ['Slowbro'].
pn([gloss: magnemite,  num:sing], magnemite, pn(magnemite))   --> ['Magnemite'].
pn([gloss: magneton,  num:sing], magneton, pn(magneton))   --> ['Magneton'].
pn([gloss: farfetch,  num:sing], farfetch, pn(farfetch))   --> ['Farfetch'].
pn([gloss: doduo,  num:sing], doduo, pn(doduo))   --> ['Doduo'].
pn([gloss: dodrio,  num:sing], dodrio, pn(dodrio))   --> ['Dodrio'].
pn([gloss: seel,  num:sing], seel, pn(seel))   --> ['Seel'].
pn([gloss: dewgong,  num:sing], dewgong, pn(dewgong))   --> ['Dewgong'].
pn([gloss: grimer,  num:sing], grimer, pn(grimer))   --> ['Grimer'].
pn([gloss: muk,  num:sing], muk, pn(muk))   --> ['Muk'].
pn([gloss: shellder,  num:sing], shellder, pn(shellder))   --> ['Shellder'].
pn([gloss: cloyster,  num:sing], cloyster, pn(cloyster))   --> ['Cloyster'].
pn([gloss: gastly,  num:sing], gastly, pn(gastly))   --> ['Gastly'].
pn([gloss: haunter,  num:sing], haunter, pn(haunter))   --> ['Haunter'].
pn([gloss: gengar,  num:sing], gengar, pn(gengar))   --> ['Gengar'].
pn([gloss: onix,  num:sing], onix, pn(onix))   --> ['Onix'].
pn([gloss: drowzee,  num:sing], drowzee, pn(drowzee))   --> ['Drowzee'].
pn([gloss: hypno,  num:sing], hypno, pn(hypno))   --> ['Hypno'].
pn([gloss: krabby,  num:sing], krabby, pn(krabby))   --> ['Krabby'].
pn([gloss: kingler,  num:sing], kingler, pn(kingler))   --> ['Kingler'].
pn([gloss: voltorb,  num:sing], voltorb, pn(voltorb))   --> ['Voltorb'].
pn([gloss: electrode,  num:sing], electrode, pn(electrode))   --> ['Electrode'].
pn([gloss: exeggcute,  num:sing], exeggcute, pn(exeggcute))   --> ['Exeggcute'].
pn([gloss: exeggutor,  num:sing], exeggutor, pn(exeggutor))   --> ['Exeggutor'].
pn([gloss: cubone,  num:sing], cubone, pn(cubone))   --> ['Cubone'].
pn([gloss: marowak,  num:sing], marowak, pn(marowak))   --> ['Marowak'].
pn([gloss: hitmonlee,  num:sing], hitmonlee, pn(hitmonlee))   --> ['Hitmonlee'].
pn([gloss: hitmonchan,  num:sing], hitmonchan, pn(hitmonchan))   --> ['Hitmonchan'].
pn([gloss: lickitung,  num:sing], lickitung, pn(lickitung))   --> ['Lickitung'].
pn([gloss: koffing,  num:sing], koffing, pn(koffing))   --> ['Koffing'].
pn([gloss: weezing,  num:sing], weezing, pn(weezing))   --> ['Weezing'].
pn([gloss: rhyhorn,  num:sing], rhyhorn, pn(rhyhorn))   --> ['Rhyhorn'].
pn([gloss: rhydon,  num:sing], rhydon, pn(rhydon))   --> ['Rhydon'].
pn([gloss: chansey,  num:sing], chansey, pn(chansey))   --> ['Chansey'].
pn([gloss: tangela,  num:sing], tangela, pn(tangela))   --> ['Tangela'].
pn([gloss: kangaskhan,  num:sing], kangaskhan, pn(kangaskhan))   --> ['Kangaskhan'].
pn([gloss: horsea,  num:sing], horsea, pn(horsea))   --> ['Horsea'].
pn([gloss: seadra,  num:sing], seadra, pn(seadra))   --> ['Seadra'].
pn([gloss: goldeen,  num:sing], goldeen, pn(goldeen))   --> ['Goldeen'].
pn([gloss: seaking,  num:sing], seaking, pn(seaking))   --> ['Seaking'].
pn([gloss: staryu,  num:sing], staryu, pn(staryu))   --> ['Staryu'].
pn([gloss: starmie,  num:sing], starmie, pn(starmie))   --> ['Starmie'].
pn([gloss: mrmime,  num:sing], mrmime, pn(mrmime))   --> ['MrMime'].
pn([gloss: scyther,  num:sing], scyther, pn(scyther))   --> ['Scyther'].
pn([gloss: jynx,  num:sing], jynx, pn(jynx))   --> ['Jynx'].
pn([gloss: electabuzz,  num:sing], electabuzz, pn(electabuzz))   --> ['Electabuzz'].
pn([gloss: magmar,  num:sing], magmar, pn(magmar))   --> ['Magmar'].
pn([gloss: pinsir,  num:sing], pinsir, pn(pinsir))   --> ['Pinsir'].
pn([gloss: tauros,  num:sing], tauros, pn(tauros))   --> ['Tauros'].
pn([gloss: magikarp,  num:sing], magikarp, pn(magikarp))   --> ['Magikarp'].
pn([gloss: gyarados,  num:sing], gyarados, pn(gyarados))   --> ['Gyarados'].
pn([gloss: lapras,  num:sing], lapras, pn(lapras))   --> ['Lapras'].
pn([gloss: ditto,  num:sing], ditto, pn(ditto))   --> ['Ditto'].
pn([gloss: eevee,  num:sing], eevee, pn(eevee))   --> ['Eevee'].
pn([gloss: vaporeon,  num:sing], vaporeon, pn(vaporeon))   --> ['Vaporeon'].
pn([gloss: jolteon,  num:sing], jolteon, pn(jolteon))   --> ['Jolteon'].
pn([gloss: flareon,  num:sing], flareon, pn(flareon))   --> ['Flareon'].
pn([gloss: porygon,  num:sing], porygon, pn(porygon))   --> ['Porygon'].
pn([gloss: omanyte,  num:sing], omanyte, pn(omanyte))   --> ['Omanyte'].
pn([gloss: omastar,  num:sing], omastar, pn(omastar))   --> ['Omastar'].
pn([gloss: kabuto,  num:sing], kabuto, pn(kabuto))   --> ['Kabuto'].
pn([gloss: kabutops,  num:sing], kabutops, pn(kabutops))   --> ['Kabutops'].
pn([gloss: aerodactyl,  num:sing], aerodactyl, pn(aerodactyl))   --> ['Aerodactyl'].
pn([gloss: snorlax,  num:sing], snorlax, pn(snorlax))   --> ['Snorlax'].
pn([gloss: articuno,  num:sing], articuno, pn(articuno))   --> ['Articuno'].
pn([gloss: zapdos,  num:sing], zapdos, pn(zapdos))   --> ['Zapdos'].
pn([gloss: moltres,  num:sing], moltres, pn(moltres))   --> ['Moltres'].
pn([gloss: dratini,  num:sing], dratini, pn(dratini))   --> ['Dratini'].
pn([gloss: dragonair,  num:sing], dragonair, pn(dragonair))   --> ['Dragonair'].
pn([gloss: dragonite,  num:sing], dragonite, pn(dragonite))   --> ['Dragonite'].
pn([gloss: mewtwo,  num:sing], mewtwo, pn(mewtwo))   --> ['Mewtwo'].
pn([gloss: mew,  num:sing], mew, pn(mew))   --> ['Mew'].



% verbs
v([gloss:sleep,  num:sing,pers:3,subj:dp(_), cpl:[]], sleep(_), v(sleep)) --> [sleeps].
v([gloss:sleep,  num:plur,pers:3,subj:dp(_), cpl:[]], sleep(_), v(sleep)) --> [sleep].

v([gloss:play,  num:sing,pers:3,subj:dp(_),  cpl:[]], play(_), v(play)) --> [plays].
v([gloss:play,  num:plur,pers:3,subj:dp(_),  cpl:[]], play(_), v(play)) --> [play].

v([gloss:like,  num:sing,pers:3,subj:dp(_),  cpl:[dp(_)]], like(_,_), v(like)) --> [likes].
v([gloss:like,  num:plur,pers:3,subj:dp(_),  cpl:[dp(_)]], like(_,_), v(like)) --> [like].

v([gloss:dream,num:sing,pers:3,subj:dp(_),   cpl:[pp(of)]],   dream(_,_),   v(dream)) --> [dreams].
v([gloss:dream,num:plur,pers:3,subj:dp(_),   cpl:[pp(of)]],   dream(_,_),   v(dream)) --> [dream].
v([gloss:dream,num:sing,pers:3,subj:dp(_),   cpl:[cp(that)]], dream(_,_),   v(dream)) --> [dreams].
v([gloss:dream,num:plur,pers:3,subj:dp(_),   cpl:[cp(that)]], dream(_,_),   v(dream)) --> [dream].

v([gloss:believe,num:sing,pers:3,subj:dp(_), cpl:[cp(that)]], believe(_,_), v(believe)) --> [believes].
v([gloss:believe,num:plur,pers:3,subj:dp(_), cpl:[cp(that)]], believe(_,_), v(believe)) --> [believe].

v([gloss:give, num:sing,pers:3,subj:dp(_),   cpl:[dp(_),dp(_)]],give(_,_,_), v(give)) --> [gives].
v([gloss:give, num:plur,pers:3,subj:dp(_),   cpl:[dp(_),dp(_)]],give(_,_,_), v(give)) --> [give].

v([gloss:talk,num:sing,pers:3,subj:dp(_),    cpl:[pp(with), pp(about)]], talk(_,_,_), v(talk)) --> [talks].
v([gloss:talk,num:plur,pers:3,subj:dp(_),    cpl:[pp(with), pp(about)]], talk(_,_,_), v(talk)) --> [talk].


v([gloss:look,num:sing,pers:3,subj:dp(_), cpl:[adj(_)]], look, v(look)) --> [looks].
v([gloss:look,num:plur,pers:3,subj:dp(_), cpl:[adj(_)]], look, v(look)) --> [look].

v([gloss:be,num:sing,pers:3,subj:dp(_),   cpl:[adj(_)]], be, v(be)) --> [is].
v([gloss:be,num:plur,pers:3,subj:dp(_),   cpl:[adj(_)]], be, v(be)) --> [are].
v([gloss:be,num:sing,pers:3,subj:dp(_),   cpl:[pp(in)]], be, v(be)) --> [is].
v([gloss:be,num:plur,pers:3,subj:dp(_),   cpl:[pp(in)]], be, v(be)) --> [are].

v([gloss:rain,num:sing,pers:3, subj:[it],  cpl:[]], [rain], v(rain)) --> [rains].


% verb used in the Pokemon example

v([gloss:fight,num:sing,pers:3,subj:dp(_),    cpl:[pp(with)]], fight(_,_), v(fight)) --> [fights].
v([gloss:fight,num:plur,pers:3,subj:dp(_),    cpl:[pp(with)]], fight(_,_), v(fight)) --> [fight].



% adj.
adj([gloss:nice],  nice(_),  adj(nice))  --> [nice].
adj([gloss:small], small(_), adj(small)) --> [small].
adj([gloss:large], large(_), adj(large)) --> [large].
adj([gloss:big],   big(_),   adj(big))   --> [big].
adj([gloss:happy], happy(_), adj(happy)) --> [happy].
adj([gloss:quiet], quiet(_), adj(quiet)) --> [quiet].
adj([gloss:black], black(_), adj(black)) --> [black].
adj([gloss:white], white(_), adj(white)) --> [white].

% adj used in the Pokemon example

adj([gloss:bug,   num:sing], bug(_),   adj(bug))   --> [bug].
adj([gloss:dragon,   num:sing], dragon(_),   adj(dragon))   --> [dragon].
adj([gloss:eletric,   num:sing], eletric(_),   adj(eletric))   --> [eletric].
adj([gloss:fighting,   num:sing], fighting(_),   adj(fighting))   --> [fighting].
adj([gloss:fire,   num:sing], fire(_),   adj(fire))   --> [fire].
adj([gloss:flying,   num:sing], flying(_),   adj(flying))   --> [flying].
adj([gloss:ghost,   num:sing], ghost(_),   adj(ghost))   --> [ghost].
adj([gloss:grass,   num:sing], grass(_),   adj(grass))   --> [grass].
adj([gloss:ground,   num:sing], ground(_),   adj(ground))   --> [ground].
adj([gloss:ice,   num:sing], ice(_),   adj(ice))   --> [ice].
adj([gloss:normal,   num:sing], normal(_),   adj(normal))   --> [normal].
adj([gloss:poison,   num:sing], poison(_),   adj(poison))   --> [poison].
adj([gloss:psychic,   num:sing], psychic(_),   adj(psychic))   --> [psychic].
adj([gloss:rock,   num:sing], rock(_),   adj(rock))   --> [rock].
adj([gloss:water,   num:sing], water(_),   adj(water))   --> [water].

% prep
p([gloss:on],   p(on))     --> [on].
p([gloss:in],   p(in))     --> [in].
p([gloss:with], p(with))   --> [with].
p([gloss:about],p(about))  --> [about].
p([gloss:of],   p(of))     --> [of].
p([gloss:to],   p(to))     --> [to].

% complementizer
c([gloss:that], c(that))   --> [that].
c([gloss:when], c(when))   --> [when].
