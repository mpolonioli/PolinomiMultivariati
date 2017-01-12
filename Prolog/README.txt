% ----------------------------------------------------------------------
%
%				MEMBRI DEL GRUPPO
%
% ----------------------------------------------------------------------
%
%%%% Polonioli Michele 780783
%%%% Nani Edoardo 780755
%%%% Cantù Stefano 763529
%
% ----------------------------------------------------------------------
%
%				SPECIFICHE DEL PROGETTO
%
% ----------------------------------------------------------------------
%
%	== INTRODUZIONE ==
%	Una delle piu' importanti applicazioni dei calcolatori fu la
%	manipolazione simbolica di operazioni matematice.
%
%	In particolare, i sistemi noti come Computer Algebra Systems
%	(cfr., Mathematica, Maple, Maxima, Axiom, etc.) si preoccupano
%	di fornire funzionalita' per la manipolazione di polinomi
%	multivariati.
%
%	Lo scopo di questo progetto e' la costruzione di una libreria
%	in Prolog per la manipolazione di polinomi multivariati
%
% ----------------------------------------------------------------------
%
%				CARICAMENTO DELLA LIBRERIA
%
% ----------------------------------------------------------------------
%
%	1 - Eseguire l'interprete Prolog nella cartella in cui è
%	    presente il file "mvpoli.pl"
%
%	2 - Eseguire il predicato: consult("mvpoli.pl").
%
% ----------------------------------------------------------------------
%
%				PREDICATI
%
% ----------------------------------------------------------------------
%
% Nome: is_varpower
%
% Sintassi:
%	is_varpower(+Varpower)
%
% Semantica:
%	Vero quando Varpower e' un termine che rappresenta la struttura
%	di una variabile
%
% Esempi:
%	is_varpower(v(1, x)). --> true.
%
% ----------------------------------------------------------------------
%
% Nome: is_monomial
%
% Sintassi:
%	is_monomial(+Monomial)
%
% Semantica:
%	Vero quando Monomial e' un termine che rappresenta la struttura
%	di un monomio
%
% Esempi:
%	is_monomial(m(1, 1, [v(1, x)])). --> true.
%
% ----------------------------------------------------------------------
%
% Nome: is_polynomial
%
% Sintassi:
%	is_polynomial(+Poly)
%
% Semantica:
%	Vero quando Poly e' un termine che rappresenta la struttura
%	di un polinomio
%
% Esempi:
%	 is_polynomial(poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])])).
%		--> true.
%
% ----------------------------------------------------------------------
%
% Nome: as_monomial
%
% Sintassi:
%	as_monomial(+Espressione, -Monomial)
%
% Semantica:
%	Vero quando Monomial e' un termine che rappresenta la struttura
%	di un monomio risultante dal "parsing" di Espressione
%
% Esempi:
%	as_monomial(42 * a^2, M).
%		--> M = m(42, 2, [v(2, a)]).
%
% ----------------------------------------------------------------------
%
% Nome: as_polynomial
%
% Sintassi:
%	as_polynomial(+Espressione, -Poly)
%
% Semantica:
%	Vero quando Poly e' un termine che rappresenta la struttura
%	di un polinomio risultante dal "parsing" di Espressione
%
% Esempi:
%	as_polynomial(a + b, P).
%		--> P = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
% ----------------------------------------------------------------------
%
% Nome: coefficients
%
% Sintassi:
%	coefficients(+Poly, -List)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio o un espressione che "parsata" rappresenta una
%	struttura polinomio e List e' la lista dei coefficienti di Poly
%
% Esempi:
%	coefficients(poly([m(42, 1, [v(1, a)])]), L). --> L = [42].
%	coefficients(m(42, 1, [v(1, a)]), L). --> L = [42].
%	coefficients(42 * a, L). --> L = [42].
%
% ----------------------------------------------------------------------
%
% Nome: variables
%
% Sintassi:
%	variables(+Poly, -List)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio o un espressione che "parsata" rappresenta una
%	struttura polinomio e List e' la lista dei simboli di variabile
%	contenuti in Poly, ordinata lessicogradicamente senza duplicati
%
% Esempi:
%	variables(poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1,b)])]), L).
%		--> L = [a, b].
%	variables(m(1, 1, [v(1, a), v(1, b)]), L).
%		--> L = [a, b].
%	variables(a + b, L).
%		--> L = [a, b].
%
% ----------------------------------------------------------------------
%
% Nome: monomials
%
% Sintassi:
%	monomials(+Poly, -List)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio o un espressione che "parsata" rappresenta una
%	struttura polinomio e List e' la lista dei monomi che compaiono
%	in Poly
%
% Esempi:
%	monomials(poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1,b)])]), L).
%		--> L = [m(1, 1, [v(1, a)]), m(1, 1, [v(1,b)])].
%	monomials(m(1, 1, [v(1, a), v(1, b)]), L).
%		--> L = [m(1, 1, [v(1, a), v(1, b)])].
%	monomials(a + b, L).
%		--> L = [m(1, 1, [v(1, a)]), m(1, 1, [v(1,b)])].
%
% ----------------------------------------------------------------------
%
% Nome: maxdegree
%
% Sintassi:
%	maxdegree(+Poly, -Degree)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio o un espressione che "parsata" rappresenta una
%	struttura polinomio e Degree e' il massimo grado di monomio
%	che compare in Poly
%
% Esempi:
%	maxdegree(poly([m(1, 2, [v(2, a)]), m(1, 1 [v(1,b)])]), D).
%		--> D = 2.
%	maxdegree(m(1, 2, [v(2, a)]), D).
%		--> D = 2.
%	maxdegree(a^2 + b, D).
%		--> D = 2.
%
% ----------------------------------------------------------------------
%
% Nome: mindegree
%
% Sintassi:
%	mindegree(+Poly, -Degree)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio o un espressione che "parsata" rappresenta una
%	struttura polinomio e Degree e' il minimo grado di monomio
%	che compare in Poly
%
% Esempi:
%	mindegree(poly([m(1, 2, [v(2, a)]), m(1, 1 [v(1,b)])]), D).
%		--> D = 1.
%	mindegree(m(1, 2, [v(2, a)]), D).
%		--> D = 2.
%	mindegree(a^2 + b, D).
%		--> D = 1.
%
% ----------------------------------------------------------------------
%
% Nome: polyplus
%
% Sintassi:
%	polyplus(+Poly1, +Poly2, -Result)
%
% Semantica:
%	Vero quando Poly1 e Poly2 sono strutture che rappresentano
%	un polinomio o un monomio o un espressione che "parsata"
%	rappresenta una struttura polinomio e Result e' la struttura
%	polinomio risultante dalla somma di Poly1 + Poly2
%
% Esempi:
%	polyplus(poly([m(1, 1, [v(1, a)])]),
%		 poly([m(1, 1, [v(1, b)])]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(m(1, 1, [v(1, a)]),
%		 m(1, 1, [v(1, b)]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(a, b, R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(m(1, 1, [v(1, a)]),
%		 poly([m(1, 1, [v(1, b)])]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(poly([m(1, 1, [v(1, a)])]),
%		 m(1, 1, [v(1, b)]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(a, poly([m(1, 1, [v(1, b)])]), R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(poly([m(1, 1, [v(1, a)])]), b, R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(m(1, 1, [v(1, a)]), b, R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
%	polyplus(a, m(1, 1, [v(1, b)]), R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%
% ----------------------------------------------------------------------
%
% Nome: polyminus
%
% Sintassi:
%	polyminus(+Poly1, +Poly2, -Result)
%
% Semantica:
%	Vero quando Poly1 e Poly2 sono strutture che rappresentano
%	un polinomio o un monomio o un espressione che "parsata"
%	rappresenta una struttura polinomio e Result e' la struttura
%	polinomio risultante dalla differenza di Poly1 - Poly2
%
% Esempi:
%	polyminus(poly([m(1, 1, [v(1, a)])]),
%		 poly([m(1, 1, [v(1, b)])]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(m(1, 1, [v(1, a)]),
%		 m(1, 1, [v(1, b)]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(a, b, R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(m(1, 1, [v(1, a)]),
%		 poly([m(1, 1, [v(1, b)])]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(poly([m(1, 1, [v(1, a)])]),
%		 m(1, 1, [v(1, b)]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(a, poly([m(1, 1, [v(1, b)])]), R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(poly([m(1, 1, [v(1, a)])]), b, R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(m(1, 1, [v(1, a)]), b, R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
%	polyminus(a, m(1, 1, [v(1, b)]), R).
%		--> R = poly([m(1, 1, [v(1, a)]), m(-1, 1, [v(1, b)])]).
%
% ----------------------------------------------------------------------
%
% Nome: polytimes
%
% Sintassi:
%	polytimes(+Poly1, +Poly2, -Result)
%
% Semantica:
%	Vero quando Poly1 e Poly2 sono strutture che rappresentano
%	un polinomio o un monomio o un espressione che "parsata"
%	rappresenta una struttura polinomio e Result e' la struttura
%	polinomio risultante dal prodotto di Poly1 * Poly2
%
% Esempi:
%	polytimes(poly([m(1, 1, [v(1, a)])]),
%		 poly([m(1, 1, [v(1, b)])]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(m(1, 1, [v(1, a)]),
%		 m(1, 1, [v(1, b)]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(a, b, R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(m(1, 1, [v(1, a)]),
%		 poly([m(1, 1, [v(1, b)])]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(poly([m(1, 1, [v(1, a)])]),
%		 m(1, 1, [v(1, b)]),
%		 R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(a, poly([m(1, 1, [v(1, b)])]), R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(poly([m(1, 1, [v(1, a)])]), b, R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(m(1, 1, [v(1, a)]), b, R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
%	polytimes(a, m(1, 1, [v(1, b)]), R).
%		--> R = poly([m(1, 1, [v(1, a), v(1, b)])]).
%
% ----------------------------------------------------------------------
%
% Nome: polyval
%
% Sintassi:
%	polyval(+Poly, +VariablesValues -R)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio o un espressione che "parsata" rappresenta una
%	struttura polinomio, VariablesValues e' una lista di numeri e
%	R e' il valore del polinomio nel punto n-dimensionale calcolato
%	sostituendo ad ogni variabile di Poly ottenuta con variables il
%	corrispondente numero contenuto nella lista VariablesValues
%
%	La lista VariablesValues deve avere lunghezza pari o superiore
%	alla lista delle variabili ottenuta con variables
%
% Esempi:
%	polyval(poly([m(1, 2, [v(1, a), v(1, b)])]),
%		     [21, 2],
%		     R).
%		--> R = 42.
%	mindegree(m(1, 2, [v(1, a), v(1, b)]), [21, 2] D).
%		--> R = 42.
%	mindegree(a * b, [21, 2], R).
%		--> R = 42.
%
% ----------------------------------------------------------------------
% Nome: pprint_polynomial
%
% Sintassi:
%	pprint_polynomial(+Poly)
%
% Semantica:
%	Vero quando Poly e' una struttura che rappresenta un polinomio
%	o un monomio
%
%	stampa sullo standard output una rappresentazione tradizionale
%	di Poly
%
% Esempi:
%	pprint_polynomial(poly([m(42, 2, [v(1, a), v(1, b)])])).
%		--> 42 * a * b
%
%	pprint_polynomial(m(42, 2, [v(1, a), v(1, b)])).
%		--> 42 * a * b
%
% ----------------------------------------------------------------------
%
%				NOTE
%
% ----------------------------------------------------------------------
%
%	Sono stati specificati soltanto i predicati pensati per essere
%	utilizzati anche all'esterno della libreria.
%
%	La libreria contiene anche altri predicati necessari
%	per il funzionamento della stessa, ma che non sono pensati per
%	essere utilizzati al di fuori
%
% ----------------------------------------------------------------------
%
%				EOF: END-OF-FILE
%
% ----------------------------------------------------------------------
