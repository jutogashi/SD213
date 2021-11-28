/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2020                       */
/* Symbolic Natural Language Processing                          */
/*            http://teaching.dessalles.fr/CANLP                 */
/*---------------------------------------------------------------*/



% partial elementary English grammar

% --- grammar
s --> np, vp.
np --> np.
np --> det, n.        % Simple noun phrase
np --> np, pp.        % Noun phrase + prepositional phrase
vp --> v.     % Verb phrase, intransitive verb
vp --> v, np.        % Verb phrase, verb + complement: like X
vp --> v, pp.        % Verb phrase, verb + indirect complement : think of X
vp --> v, np, pp.    % Verb phrase, verb + complement + indirect complement : give X to Y
vp --> v, pp, pp.    % Verb phrase, verb + indirect complement + indirect complement : talk to X about Y
pp --> p, np.        % prepositional phrase

% -- lexicon
np --> [kirk].
det --> [the]; [my]; [her]; [his]; [a].
det --> [some].
n --> [dog]; [daughter]; [son]; [sister]; [aunt]; [neighbour]; [cousin].
v --> [grumbles]; [likes]; [gives]; [talks]; [annoys]; [hates].
v --> [cries]; [barks].
p --> [of]; [to].
p --> [about].