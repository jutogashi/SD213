
example(1,"elle _PP mange le g�teau en une minute",'************').
example(1,"elle _PP mange le g�teau en 2010",_).
example(0,"elle _PP mange le g�teau pendant une minute",_).
example(1,"elle _PP mange le g�teau pendant le spectacle",_).
%example(1,"elle _PP mange le g�teau pendant cette minute-l�",_).

example(1,"elle _PP mange du g�teau en une minute",au_bout_d_une_minute).
example(1,"elle _PP mange du g�teau en 2010", 'manger_du_gateau!').
%example(1,"elle _PP mange du g�teau, en 2010", pred:manger_gateau).
example(1,"elle _PP mange du g�teau pendant une minute", record).
example(1,"elle _PP mange du g�teau pendant le spectacle",_).
%example(1,"elle _PP mange du g�teau pendant cette minute-l�",_).

example(1,"elle _PP mange un g�teau en une minute", record).
example(1,"elle _PP mange un g�teau en 2010",_).
example(0,"elle _PP mange un g�teau pendant une minute",'*************').
example(1,"elle _PP mange un g�teau pendant le spectacle",_).

example(1,"elle _PP mange dix g�teau en une minute", record).
example(1,"elle _PP mange dix g�teau en 2010",_).
example(0,"elle _PP mange dix g�teau pendant une minute",'*************').
example(1,"elle _PP mange dix g�teau pendant le spectacle",_).

example(1,"elle _PP mange en une minute",_).
example(1,"elle _PP mange en 2010",'interpretation repetitive possible').
example(1,"elle _PP mange pendant une minute", grignoter).
example(1,"elle _PP mange pendant le spectacle",_).
%example(1,"elle _PP mange pendant cette minute-l�",_).

example(1,"elle _PP ronfle en une minute",'').
example(1,"elle _PP ronfle en 2010",'').
example(1,"elle _PP ronfle pendant une minute",'').
example(1,"elle _PP ronfle pendant le spectacle",'').

example(1,"elle _PP conduire une voiture pendant une minute",_).
example(1,"elle _PP conduire la voiture pendant une minute",_).
