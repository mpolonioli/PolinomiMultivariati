%%% -*- Mode: Prolog -*-
%%	polinomi.pl


% monomio = m(Coefficent, TotalDegree, VarsPowers)
% variabile = v(Power, VarSymbol)
% polinomio = poly(Monomials)


%%	is_varpower(VarPower)
%
%	vero se VarPower è un termine che rappresenta una variabile

is_varpower(v(Power, VarSymbol)) :-
	integer(Power),
	Power >= 0,
	is_variable_symbol(VarSymbol).


%%	is_monomial(Monomial)
%
%	vero se Monomial è un termine che rappresenta un monomio

is_monomial(m(C, TD, VPs)) :-
	    integer(TD),
	    TD >= 0,
	    is_list(VPs),
	    foreach(member(VP, VPs), is_varpower(VP)),
	    calc_total_degree(VPs, 0, TD),
	    is_number(C).


%%	is_polynomial(Poly)
%
%	vero se Poly è un termine che rappresenta un polinomio

is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).


%%	calc_total_degree(VarPower, Acc, Result)
%
%	vero quando Result è la somma delle potenze che compaioni in
%	Varpower

% caso base: variabili terminate
calc_total_degree([], R, R) :- !.

% ho ancora variabili da calcolare
calc_total_degree([v(P, _) | VPs], Acc, R) :-
	!,
	NAcc is Acc + P,
	calc_total_degree(VPs, NAcc, R).


%%      as_monomial(Expression, Monomial)
%
%	vero quando Monomial `e il termine che rappresenta il
%	monomio risultante dal “parsing” dell’espressione Expression

as_monomial(Input, m(C, TD, SVP)) :-
	parse_monomial(Input, [], 0, TD, VP, C),
	semplify_varpower(VP, SVP).


%%      as_polynomial(Expression, Polynomial)
%
%	vero quando Polynomial `e il termine che rappresenta il
%	polinomio risultante dal “parsing” dell’espressione Expression

as_polynomial(Input, poly(SSMs)) :-
	parse_polynomial(Input, [], Ms),
	semplify_polynomial(Ms, SSMs).

%%	sort_Monomials(Monomials, SortedMonomials)
%
%	vero quando SortedMonomials è la lista dei monomi Monomials
%	ordinata secondo le specifiche

sort_monomials(Ms, R) :-
	create_pairs_m_v(Ms, [], PairsMV),
	sort(2, =<, PairsMV, SPairsMV),
	get_ms_from_pairs_m_v(SPairsMV, [], SMs),
	sort(2, =<, SMs, SMs1),
	sort_playoff_ms(SMs1, R).

%%	create_pairs_m_v(Monomials, App, PairsMV)
%
%	vero quando PairsMV è una lista di coppie (Monomial, Variables)
%	creata con i monomi contenuti in Monomials

% caso base : ho terminato i monomi
create_pairs_m_v([], R, R) :- !.

% ho ancora monomi
create_pairs_m_v([m(C, TD, VP) | Ms], App, R) :-
	!,
	extract_variables(VP, [], Variables),
	append(App, [(m(C, TD, VP), Variables)], NApp),
	create_pairs_m_v(Ms, NApp, R).

%%	get_ms_from_pairs_m_v(PairsMV, App, Monomials)
%
%	vero quando Monomials è la lista dei monomi che compaiono nelle
%	coppie contenute nella lista PairsMV

% caso base: ho terminato le coppie
get_ms_from_pairs_m_v([], R, R) :- !.

% ho ancora coppie
get_ms_from_pairs_m_v([(M, _) | Pairs], App, R) :-
	!,
	append(App, [M], NApp),
	get_ms_from_pairs_m_v(Pairs, NApp, R).

%%	sort_playoff_ms(Monomials, Result)
%
%	vero quando Result è la lista dei monomi che compaiono in
%	Monomials con i monomi con stesso grado e stesse
%	variabili ordinati secondo le specifiche
%
%	Monomials deve già essere ordinata per grado totale

% caso base: lista di monomi vuota
sort_playoff_ms([], []) :- !.

% caso base: lista di monomi con un solo monomio
sort_playoff_ms([M | []], [M | []]) :- !.

% lista di monomi con almeno 2 monomi
sort_playoff_ms([M | Ms], R) :-
	!,
	sort_playoff_ms(M, Ms, [], [], R).


%%	sort_playoff_ms(Monomial, RestMs, App1, App2, Result)
%
%	predicato ausiliario di sort_playoff_ms(Monomials, Result)
%
%	Monomial è il primo monomio di Monomials e RestMs è il resto
%	di tale lista
%
%	App1 tiene in memoria i monomi che sono stati confrontati fin
%	ora con Monomial e >
%
%	App2 è la lista ordinata dei monomi incortrati

% caso base: ho terminato i monomi da confrontare
sort_playoff_ms(M, [], [], App2, R) :-
	!,
	append(App2, [M], R).

% ultimo gruppo da spareggiare con almeno un monomio
sort_playoff_ms(M, [], [FirstApp1 | RestApp1], App2, R) :-
	!,
	append(App2, [M], NApp2),
	sort_playoff_ms(FirstApp1, RestApp1, [], NApp2, R).

% ho trovato un monomio da spareggiare maggiore o uguale di quello che
% sto considerando
sort_playoff_ms(m(C1, TD1, VP1), [m(C2, TD1, VP2) | Ms], App1, App2, R) :-
	extract_variables(VP1, [], Variables),
	extract_variables(VP2, [], Variables),
	varpower_min_or_equal(VP1, VP2, 1),
	!,
	append(App1, [m(C2, TD1, VP2)], NApp1),
	sort_playoff_ms(m(C1, TD1, VP1), Ms, NApp1, App2, R).

% ho trovato un monomio da spareggiare minore di quello che sto
% considerando
sort_playoff_ms(m(C1, TD1, VP1), [m(C2, TD1, VP2) | Ms], App1, App2, R) :-
	extract_variables(VP1, [], Variables),
	extract_variables(VP2, [], Variables),
	varpower_min_or_equal(VP1, VP2, 0),
	!,
	append(App1, [m(C1, TD1, VP1)], NApp1),
	sort_playoff_ms(m(C2, TD1, VP2), Ms, NApp1, App2, R).

% ho trovato un monomio con grado maggiore di quello che sto
% considerando che è l'ultimo o l'unico del gruppo con cui devo
% spareggiarlo
sort_playoff_ms(m(C1, TD1, VP1), [m(C2, TD2, VP2) | Ms], [], App2, R) :-
	TD1 \= TD2,
	!,
	append(App2, [m(C1, TD1, VP1)], NApp2),
	sort_playoff_ms(m(C2, TD2, VP2), Ms, [], NApp2, R).

% ho trovato un monomio con grado maggiore di quello che sto
% considerando ma non ho finito di spareggiare il suo gruppo
sort_playoff_ms(m(C1, TD1, VP1),
		[m(C2, TD2, VP2) | Ms],
		[m(C3, TD3, VP3) | RestApp1],
		App2,
		R) :-

	TD1 \= TD2,
	!,
	append(App2, [m(C1, TD1, VP1)], NApp2),
	append(RestApp1, [m(C2, TD2, VP2) | Ms], NMs),
	sort_playoff_ms(m(C3, TD3, VP3), NMs, [], NApp2, R).

% ho trovato un monomio con grado uguale di quello che sto
% considerando ma simboli di varibile diversi ed è l'ultimo o l'unico
% del gruppo con cui devo spareggiarlo
sort_playoff_ms(m(C1, TD1, VP1), [m(C2, TD1, VP2) | Ms], [], App2, R) :-
	extract_variables(VP1, [], Variables1),
	extract_variables(VP2, [], Variables2),
	Variables1 \= Variables2,
	!,
	append(App2, [m(C1, TD1, VP1)], NApp2),
	sort_playoff_ms(m(C2, TD1, VP2), Ms, [], NApp2, R).

% ho trovato un monomio con grado uguale di quello che sto
% considerando ma simboli di varibile diversi ma non ho finito di
% spareggiare il suo gruppo
sort_playoff_ms(m(C1, TD1, VP1),
		[m(C2, TD1, VP2) | Ms],
		[m(C3, TD3, VP3) | RestApp1],
		App2,
		R) :-

	extract_variables(VP1, [], Variables1),
	extract_variables(VP2, [], Variables2),
	Variables1 \= Variables2,
	!,
	append(App2, [m(C1, TD1, VP1)], NApp2),
	append(RestApp1, [m(C2, TD1, VP2) | Ms], NMs),
	sort_playoff_ms(m(C3, TD3, VP3), NMs, [], NApp2, R).


%%	varpower_min_or_equal(VP1, VP2, Result)
%
%	predicato ausiliario di sort_playoff_ms/5
%
%	VP1 e VP2 devono essere liste di variabili con simboli uguali e
%	somma dei gradi uguali
%
%	vero quando (Result = 1 e VP1 <= VP2) oppure
%		    (Result = 0 e VP1 > VP2)

% caso base: VP1 < VP2
varpower_min_or_equal([v(P1, S) |  _], [v(P2, S) | _], 1) :-
	P1 < P2,
	!.

% caso base: VP1 = VP2
varpower_min_or_equal([], [], 1) :- !.

% caso base: VP1 > VP2
varpower_min_or_equal([v(P1, S) | _],[v(P2, S) | _], 0) :-
	P1 > P2,
	!.

% ho trovato 2 variabili identiche
varpower_min_or_equal([v(P, S) | VPs1], [v(P, S) | VPs2], R) :-
	!,
	varpower_min_or_equal(VPs1, VPs2, R).


%%	semplify_polynomial(Poly1, Poly2)
%
%	vero se Poly2 è Poly1 semplificato

% la semplifica della lista vuota è la lista vuota
semplify_polynomial([], []) :- !.

% chiama semplify_polynomial/5 e zero_eater_m/3 per togliere eventuali
% monomi con coefficente nullo
semplify_polynomial([m(C, TD, VP) | Ms], R) :-
	!,
	semplify_varpower(VP, SVP),
	semplify_polynomial(m(C, TD, SVP), Ms, [], [], SMs),
	zero_eater_m(SMs, [], SMsWithoutZeros),
	sort_monomials(SMsWithoutZeros, R).


%%	semplyfy_polynomial(Monomial, Monomials, App, MsApp, Resut)
%
%       predicato ausiliario di semplify_polynomial/2
%
%	Monomial rappresenta il monomio da semplificare con la lista di
%	monomi contenuti in Monomials
%
%	App è una lista d'apoggio in cui vengono concatenati soltanto i
%	monomi che vanno presi in considerazione per una successiva
%	semplifica
%
%	Result è la lista dei monomi semplificati

% caso base: la semplifica del monomio è terminata e non mi rimangono
% monomi da seplificare
semplify_polynomial(m(C, TD, VP), [], [], MsApp, RMs) :-
	!,
	append(MsApp, [m(C, TD, VP)], RMs).

% la semplifica del monomio è terminata e ho ancora monomi da
% semplificare
semplify_polynomial(m(C, TD, VP), [], [m(C1, TD1, VP1) | Ms], MsApp, R) :-
	!,
	append(MsApp, [m(C, TD, VP)], NMsApp),
	semplify_polynomial(m(C1, TD1, VP1), Ms, [], NMsApp, R).

% monomi con variabili identiche
semplify_polynomial(m(C1, TD, VP1), [m(C2, TD, VP2) | Ms], App, MsApp, RMs) :-
	semplify_varpower(VP1, VP2),
	!,
	NC is C1 + C2,
	semplify_polynomial(m(NC, TD, VP1), Ms, App, MsApp, RMs).

% monomi con variabili diverse
semplify_polynomial(m(C1, TD1, VP1), [m(C2, TD2, VP2) | Ms], App, MsApp, RMs) :-
	semplify_varpower(VP2, SSVP2),
	VP1 \= SSVP2,
	!,
	append(App, [m(C2, TD2, SSVP2)], NApp),
	semplify_polynomial(m(C1, TD1, VP1), Ms, NApp, MsApp, RMs).


%%	semplify_varpower(VarPower, Result)
%
%	vero quando Result è la lista delle variabili che compaioni in
%	VarPower semplificate e riordinate secondo le specifiche

% ordino e chiamo semplify_varpower/3
semplify_varpower(VP, R) :-
	!,
	sort(2, =<, VP, SVP),
	semplify_varpower(SVP, [], R).


%%	semplify_varpower(VarPower, App, Result)
%
%	predicato ausiliario di semplyfy_varpower/2
%
%	esegue la semplifica di VarPower, ammesso che sia una lista
%	ordinata secondo le specifiche

% variabili da semplificare
semplify_varpower([v(P1, S), v(P2, S) | Vs], App, R) :-
	!,
	P is P1 + P2,
	semplify_varpower([v(P, S) | Vs], App, R).

% variabili non da semplificare
semplify_varpower([v(P1, S1), v(P2, S2) | VPs], App, R) :-
	S1 \= S2,
	!,
	append([v(P1, S1)], App, NApp),
	semplify_varpower([v(P2, S2) | VPs], NApp, R).

% ultima variabile
semplify_varpower([V | []], App, R) :-
	!,
	append([V], App, NApp),
	reverse(NApp, SVP),
	zero_eater_vp(SVP, [], R).

% nessuna variabile
semplify_varpower([], [], []) :- !.


%%	zero_eater_vp(VarPower, App, Result)
%
%	vero se Result è la lista delle variabili che compaioni in
%	VarPower tranne quelle con esponente nullo

% ho finito la lista
zero_eater_vp([], App, R) :-
	!,
	reverse(App, R).

% variablie con esponente nullo
zero_eater_vp([v(0, _) | VPs], App, R) :-
	!,
	zero_eater_vp(VPs, App, R).

% variabile con esponente non nullo
zero_eater_vp([v(P, S) | VPs], App, R) :-
	P \= 0,
	!,
	append([v(P, S)], App, NApp),
	zero_eater_vp(VPs, NApp, R).


%%	zero_eater_m(Monomials, App, Result)
%
%	vero se Result è la lista dei monomi che compaioni in Monomials
%	tranne quelli con coefficente nullo

% ho finito la lista
zero_eater_m([], R, R) :- !.

% monomio con coefficiente nullo
zero_eater_m([m(0, _, _) | Ms], App, R) :-
	!,
	zero_eater_m(Ms, App, R).

zero_eater_m([m(0.0, _, _) | Ms], App, R) :-
	!,
	zero_eater_m(Ms, App, R).


% monomio con coefficiente non nullo
zero_eater_m([m(C, TD, VP) | Ms], App, R) :-
	% C \= 0,
	!,
	append(App, [m(C, TD, VP)], NApp),
	zero_eater_m(Ms, NApp, R).


%%	parse_monomial(Input, AppVP, AppTD, TotalDegree, VarPower,
%		       Coefficent)
%
%	vero quando Input è un monomio scritto nella forma corretta
%	TotalDegree il suo grado totale
%
%	VarPower la lista delle sue variabili nella forma v(P, S)
%
%	Coefficent il suo coefficente.

% variabile senza esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, C) :-
	Input =.. [*, Y, Z],
	is_variable_symbol(Z),
	!,
	append(AppVP, [v(1, Z)], NAppVP),
	NAppTD is AppTD + 1,
	parse_monomial(Y, NAppVP, NAppTD, RTD, RVP, C).

% variabile con esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, C) :-
	Input =.. [*, Y, Z],
	Z =.. [^, Y1, Z1],
	is_variable_symbol(Y1),
	integer(Z1),
	>=(Z1, 0),
	!,
	append(AppVP, [v(Z1, Y1)], NVP),
	NAppTD is AppTD + Z1,
	parse_monomial(Y, NVP, NAppTD, RTD, RVP, C).

% nessun coefficiente
% segno "+" davanti al monomio
% variabile senza esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, 1) :-
	Input =.. [+, Y],
	is_variable_symbol(Y),
	!,
	RTD is AppTD + 1,
	append(AppVP, [v(1, Y)], RVP).

% nessun coefficiente
% segno "+" davanti al monomio
% variabile con esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, 1) :-
	Input =.. [+, V],
	V =.. [^, Y, Z],
	is_variable_symbol(Y),
	integer(Z),
	>=(Z, 0),
	!,
	RTD is AppTD + Z,
	append(AppVP, [v(Z, Y)], RVP).

% nessun coefficiente
% segno "-" davanti al monomio
% variabile senza esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, -1) :-
	Input =.. [-, Y],
	is_variable_symbol(Y),
	!,
	RTD is AppTD + 1,
	append(AppVP, [v(1, Y)], RVP).

% nessun coefficiente
% segno "-" davanti al monomio
% variabile con esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, -1) :-
	Input =.. [-, V],
	V =.. [^, Y, Z],
	is_variable_symbol(Y),
	integer(Z),
	>=(Z, 0),
	!,
	RTD is AppTD + Z,
	append(AppVP, [v(Z, Y)], RVP).

% nessun coefficiente
% nessun segno davanti al monomio
% variabile senza esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, 1) :-
	Input =.. [X],
	is_variable_symbol(X),
	!,
	RTD is AppTD + 1,
	append(AppVP, [v(1, X)], RVP).

% nessun coefficiente
% nessun segno davanti al monomio
% variabile con esponente
parse_monomial(Input, AppVP, AppTD, RTD, RVP, 1) :-
	Input =.. [^, Y, Z],
	is_variable_symbol(Y),
	integer(Z),
	>=(Z, 0),
	!,
	RTD is AppTD + Z,
	append(AppVP, [v(Z, Y)], RVP).

% coefficente
parse_monomial(Input, RVP, RTD, RTD, RVP, C) :-
	arithmetic_expression_value(Input, C),
	!.


%%	parse_polynomial(Input, App, Monomials)
%
%	vero se Input è un polinomio scritto correttamente e Monomials
%	la lista dei suoi monomi nella forma [m(C, TD, VP) | Ms]

% ho ancora monomi e quello preso in considerazione ha coefficente "+"
parse_polynomial(Input, App, RM) :-
	Input =.. [+, Y, Z],
	!,
	as_monomial(Z, M),
	append(App, [M], NApp),
	parse_polynomial(Y, NApp, RM).


% ho ancora monomi e quello preso in considerazione ha coefficente "-"
parse_polynomial(Input, App, RM) :-
	Input =.. [-, Y, Z],
	!,
	as_monomial(Z, m(C, TD, VP)),
	NC is -C,
	append(App, [m(NC, TD, VP)], NApp),
	parse_polynomial(Y, NApp, RM).

% primo monomio
parse_polynomial(Input, App, RM) :-
	% not_is_sign(X),
	!,
	as_monomial(Input, M),
	append(App, [M], RM).


%%	is_variable_symbol(Symbol)
%
%       vero se Symbol è un simbolo di variabile accettabile

% un numero intero o con virgola
is_variable_symbol(X) :-
	atomic(X),
	is_number(X),
	!,
	fail.

% qualsiasi atomo che non sia un numero intero o con virgola
is_variable_symbol(X) :-
	atomic(X),
	%not_is_number(X),
	!.


%%	is_number(Number)
%
%       vero se Number è un intero o un numero con virgola

% numero intero
is_number(X) :-
	integer(X),
	!.

% numero con virgola
is_number(X) :-
	float(X),
	!.


%%	coefficients(Poly, Coefficients)
%
%	vero quando Coefficients è una lista dei coefficienti di Poly

% caso base: un polinomio che non ha monomi torna la lista vuota
coefficients(poly([]), []) :- !.

% input in forma m
coefficients(m(C, TD, VP), Coefficient) :-
	!,
	coefficients(poly([m(C, TD, VP)]), Coefficient).

% input in forma poly
coefficients(poly(Ms), Coefficients) :-
	is_polynomial(poly(Ms)),
	!,
	semplify_polynomial(Ms, SSMs),
	coefficients(poly(SSMs), [], Coefficients).

% input in forma "scomoda"
coefficients(Poly, Coefficients) :-
	% not_is_polynomial(Poly),
	!,
	as_polynomial(Poly, Poly1),
	coefficients(Poly1, [], Coefficients).


%%	coefficients(Poly, App, Coefficients)
%
%       predicato ausiliario di coefficients/2

% caso base: ho terminato i monomi
coefficients(poly([]), R, R) :- !.

% estraggo il coefficente del monomio in testa
coefficients(poly([m(C, _, _) | Ms]), App, R) :-
	!,
	append(App, [C], NApp),
	coefficients(poly(Ms), NApp, R).


%%	variables(Poly, Variables)
%
%	vero quando Variables è una lista dei simboli di variabile che
%	appaiono in Poly, senza ripetizioni e ordinate
%	lessicograficamente

% caso base: un polinomio che non ha monomi torna la lista vuota
variables(poly([]), []) :- !.

% input in forma m
variables(m(C, TD, VP), Variables) :-
	!,
	variables(poly([m(C, TD, VP)]), Variables).

% input in forma poly
variables(poly(Ms), Variables) :-
	is_polynomial(poly(Ms)),
	!,
	semplify_polynomial(Ms, SSMs),
	variables(poly(SSMs), [], Variables).

% input in forma "scomoda"
variables(Poly, Variables) :-
	% not_is_polynomial(Poly),
	!,
	as_polynomial(Poly, Poly1),
	variables(Poly1, [], Variables).


%%	variables(Poly, App, Variables)
%
%	predicato ausiliario di variables/2

% caso base: ho terminato i monomi
variables(poly([]), App, R) :-
	!,
	sort(App, R).

% monomio senza variabili
variables(poly([m(_, _, []) | Ms]), App, R) :-
	!,
	variables(poly(Ms), App, R).

% estraggo i simboli di variabile da un monomio
variables(poly([m(_, _, VP) | Ms]), App, R) :-
	VP \= [],
	!,
	extract_variables(VP, App, NApp),
	variables(poly(Ms), NApp, R).


%%	extract_variables(VarPower, App, Variables)
%
%	vero quando Variables è la lista dei simboli delle variabili che
%	compaiono in VarPower

% caso base: ho terminato la lista di variabili
extract_variables([], R, R) :- !.

% estraggo il simbolo della variabile in testa e passo alle successive
extract_variables([v(_, S) | VPs], App, R) :-
	!,
	append(App, [S], NApp),
	extract_variables(VPs, NApp, R).


%%	monomials(Poly, Monomials)
%
%	vero quando Monomials � la lista ordinata dei monomi che
%	compaiono in Poly

% input in forma poly
monomials(poly(Monomials), R) :-
	is_polynomial(poly(Monomials)),
	!,
	semplify_polynomial(Monomials, R).

% input in forma m
monomials(m(C, TD, VP), m(C, TD, SVP)) :-
	is_monomial(m(C, TD, VP)),
	!,
	semplify_varpower(VP, SVP).

% input in forma "scomoda"
monomials(Input, Monomials) :-
	as_polynomial(Input, poly(Monomials)),
	!.


%%	maxdegree(Poly, Degree)
%
%       vero quando Degree è il massimo grado dei monomi che appaiono in
%       Poly

% input in forma m
maxdegree(m(C, TD, VP), Degree) :-
	!,
	maxdegree(poly([m(C, TD, VP)]), Degree).

% input in forma poly e polinomio semplificato senza monomi
maxdegree(poly(Monomials), 0) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, []),
	!.

% input in forma poly e polinomio semplificato con almeno un monomio
maxdegree(poly(Monomials), Degree) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, SSMs),
	!,
	last(SSMs, m(_, Degree, _)).

% input in forma "scomoda" e polinomio parsato senza monomi
maxdegree(Input, 0) :-
	% not_is_polynomial(Input),
	as_polynomial(Input, poly([])),
	!.

% input in forma "scomoda" e polinomio parsato con almeno un monomio
maxdegree(Input, Degree) :-
	% not_is_polynomial(Input),
	as_polynomial(Input, poly(Monomials)),
	!,
	last(Monomials, m(_, Degree, _)).


%%	mindegree(Poly, Degree)
%
%	vero quando Degree è il minimo grado dei monomi che
%	appaiono in Poly

% input in forma m
mindegree(m(C, TD, VP), Degree) :-
	!,
	mindegree(poly([m(C, TD, VP)]), Degree).

% input in forma poly e polinomio semplificato senza monomi
mindegree(poly(Monomials), 0) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, []),
	!.

% input in forma poly e polinomio semplificato con almeno un monomio
mindegree(poly(Monomials), Degree) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, [m(_, Degree, _) | _]),
	!.

% input in forma "scomoda" e polinomio parsato senza monomi
mindegree(Input, 0) :-
	% not_is_polynomial(Input),
	as_polynomial(Input, poly([])),
	!.

% input in forma "scomoda" e polinomio parsato con almeno un monomio
mindegree(Input, Degree) :-
	% not_is_polynomial(Input),
	as_polynomial(Input, poly([m(_, Degree, _) | _])),
	!.


%%	poly_plus(Poly1, Poly2, Result)
%
%       vero quando Result è il polinomio somma di Poly1 + Poly2.

% input in forma m, m
polyplus(m(C1, TD1, VP1), m(C2, TD2, VP2), Result) :-
	!,
	polyplus(poly([m(C1, TD1, VP1)]), poly([m(C2, TD2, VP2)]), Result).

% input in forma poly, m
polyplus(poly(Ms), m(C, TD, VP), Result) :-
	!,
	polyplus(poly(Ms), poly([m(C, TD, VP)]), Result).

% input in forma m, poly
polyplus(m(C, TD, VP), poly(Ms), Result) :-
	!,
	polyplus(poly([m(C, TD, VP)]), poly(Ms), Result).

% input in forma poly, poly
polyplus(poly(Ms1), poly(Ms2), poly(SSMs3)) :-
	is_polynomial(poly(Ms1)),
	is_polynomial(poly(Ms2)),
	!,
	append(Ms1, Ms2, Ms3),
	semplify_polynomial(Ms3, SSMs3).

% input in forma "scomoda"
polyplus(Input1, Input2, poly(SSMs3)) :-
	% not_is_poynomial(Input1),
	% not_is_polynomial(input2),
	!,
	as_polynomial(Input1, poly(Ms1)),
	as_polynomial(Input2, poly(Ms2)),
	append(Ms1, Ms2, Ms3),
	semplify_polynomial(Ms3, SSMs3).


%%	poly_minus(Poly1, Poly2, Result)
%
%       vero quando Result è il polinomio differenza di Poly1 - Poly2.

% input in forma m, m
polyminus(m(C1, TD1, VP1), m(C2, TD2, VP2), Result) :-
	!,
	polyminus(poly([m(C1, TD1, VP1)]), poly([m(C2, TD2, VP2)]), Result).

% input in forma poly, m
polyminus(poly(Ms), m(C, TD, VP), Result) :-
	!,
	polyminus(poly(Ms), poly([m(C, TD, VP)]), Result).

% input in forma m, poly
polyminus(m(C, TD, VP), poly(Ms), Result) :-
	!,
	polyminus(poly([m(C, TD, VP)]), poly(Ms), Result).

% input in forma poly, poly
polyminus(poly(Ms1), poly(Ms2), poly(SSMs3)) :-
	is_polynomial(poly(Ms1)),
	is_polynomial(poly(Ms2)),
	!,
	invert_sign(Ms2, [], IMs2),
	append(Ms1, IMs2, Ms3),
	semplify_polynomial(Ms3, SSMs3).

% input in forma "scomoda"
polyminus(Input1, Input2, poly(SSMs3)) :-
	% not_is_poynomial(Input1),
	% not_is_polynomial(input2),
	!,
	as_polynomial(Input1, poly(Ms1)),
	as_polynomial(Input2, poly(Ms2)),
	invert_sign(Ms2, [], IMs2),
	append(Ms1, IMs2, Ms3),
	semplify_polynomial(Ms3, SSMs3).


%%	invert_sign(Monomials, App, Result)
%
%	vero quando Result è la lista Monomials con i coefficenti
%	dei monomi con segno invertito

% caso base: ho terminato i monomi da invertire
invert_sign([], R, R) :- !.

% ho ancora monomi a cui invertire il segno del coefficente
invert_sign([m(C, TD, VP) | Ms], App, R) :-
	is_monomial(m(C, TD, VP)),
	!,
	NC is C * (-1),
	append(App, [m(NC, TD, VP)], NApp),
	invert_sign(Ms, NApp, R).


%%	polytimes(Poly1, Poly2, Result)
%
%	vero quando Result è il polinomio risultante dalla
%	moltiplicazione di Poly 1 e Poly2

% input in forma m, m
polytimes(m(C1, TD1, VP1), m(C2, TD2, VP2), Result) :-
	!,
	polytimes(poly([m(C1, TD1, VP1)]), poly([m(C2, TD2, VP2)]), Result).

% input in forma poly, m
polytimes(poly(Ms), m(C, TD, VP), Result) :-
	!,
	polytimes(poly(Ms), poly([m(C, TD, VP)]), Result).

% input in forma m, poly
polytimes(m(C, TD, VP), poly(Ms), Result) :-
	!,
	polytimes(poly([m(C, TD, VP)]), poly(Ms), Result).


% Input in forma poly, poly con almeno un monomio per entrambi i
% polinomi
polytimes(poly([m(C1, TD1, VP1) | Ms1]),
	  poly([m(C2, TD2, VP2) | Ms2]),
	  poly(Ms3)) :-
	is_polynomial(poly([m(C1, TD1, VP1) | Ms1])),
	is_polynomial(poly([m(C2, TD2, VP2) | Ms2])),
	!,
	polytimes([m(C1, TD1, VP1) | Ms1],
		  [m(C2, TD2, VP2) | Ms2],
		  [m(C2, TD2, VP2) | Ms2],
		  [],
		  Ms3).

% input in forma "scomoda" con almeno un monomio per entrambi i polinomi
polytimes(Input1, Input2, poly(Ms3)) :-
	as_polynomial(Input1, poly([m(C1, TD1, VP1) | Ms1])),
	as_polynomial(Input2, poly([m(C2, TD2, VP2) | Ms2])),
	!,
	polytimes([m(C1, TD1, VP1) | Ms1],
		  [m(C2, TD2, VP2) | Ms2],
		  [m(C2, TD2, VP2) | Ms2],
		  [],
		  Ms3).

% input in forma "scomoda" con almeno un polinomio privo di monomi
polytimes(Input1, Input2, poly([])) :-
	as_polynomial(Input1, poly([])),
	!,
	as_polynomial(Input2, _).

polytimes(Input1, Input2, poly([])) :-
	as_polynomial(Input2, poly([])),
	!,
	as_polynomial(Input1, _).

% input in forma poly, poly con almeno un polinomio privo di monomi
polytimes(poly([]), Poly2, poly([])) :-
	!,
	is_polynomial(Poly2).
polytimes(Poly1, poly([]), poly([])) :-
	!,
	is_polynomial(Poly1).


%%	polytimes(Monomials1, Monomials2, Monomials2, App, Result)
%
%       predcato ausiliario di polyrimes/3
%
%	il terzo argomento è necessario per tenere in memoria i monomi
%	devo moltiplicare ogni monomio di Monomials1

% caso base: ho terminato di moltiplicare l'ultimo monomio della prima
% lista con i monomi della seconda lista
polytimes([m(_, _, _) | []], [], _, Ms, SSMs) :-
	!,
	semplify_polynomial(Ms, SSMs).

% non ho ancora finito di moltiplicare il monomio in testa della prima
% lista con i monomi della seconda lista
polytimes([m(C1, TD1, VP1) | Ms1], [m(C2, TD2, VP2) | Ms2], Save, App, R) :-
	!,
	C3 is C1 * C2,
	append(VP1, VP2, VP3),
	semplify_varpower(VP3, SSVP3),
	TD3 is TD1 + TD2,
	append(App, [m(C3, TD3, SSVP3)], NApp),
	polytimes([m(C1, TD1, VP1) | Ms1], Ms2, Save, NApp, R).

% ho finito i monomi della seconda lista quindi devo passare a
% moltiplicare il prossimo monomio della prima lista e ripristinare la
% seconda lista
polytimes([m(_, _, _) | Ms1], [], Save, App, R) :-
	!,
	polytimes(Ms1, Save, Save, App, R).


%%	polyval(Poly, VariableValues, Value)
%
%	vero quando Value contiene il valore del polinomio nel punto
%	n-dimensionale rappresentato dalla lista VariableValues che
%	contiene un valore per ogni variablie ottenuta con il predicato
%	variables/2

% input in forma m
polyval(m(C, TD, VP), VariablesValue, Value) :-
	!,
	polyval(poly([m(C, TD, VP)]), VariablesValue, Value).

% input in forma "scomoda" con polinomio non nullo
polyval(Input, VariablesValue, Value) :-
	as_polynomial(Input, poly(Monomials)),
	Monomials \= [],
	!,
	variables(poly(Monomials), Variables),
	create_pairs_KV(VariablesValue, Variables, [], CoppieKV),
	polyval(Monomials, CoppieKV, 1, 0, Value).

% input in forma "scomoda" con polinomio nullo
polyval(Input, _, 0) :-
	as_polynomial(Input, poly([])),
	!.

% input in forma poly con polinomio non nullo
polyval(poly(Monomials), VariablesValue, Value) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, SMs),
	SMs \= [],
	!,
	variables(poly(SMs), Variables),
	create_pairs_KV(VariablesValue, Variables, [], CoppieKV),
	polyval(SMs, CoppieKV, 1, 0, Value).

% input in forma poly con polinomio nullo
polyval(poly(Monomials), _, 0) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, []),
	!.


%%	polyval(Monomials, CoppieKV, AccTimes, AccPlus, Result)
%
%	prediacto ausiliario di polyval/2
%
%	vero quando Result è il risultato del calcolo della lista di
%	monomi Monomials con le variabili sostituite con i corrispettivi
%	valori contenuti nella lista CoppieKV

% ho ancora monomi da calcolare
polyval([m(C, _, VP) | Ms], CoppieKV, AccTimes, AccPlus, R) :-
	!,
	variableval(VP, CoppieKV, AccTimes, NAccTimes),
	NAccPlus is (AccPlus + (NAccTimes * C)),
	polyval(Ms, CoppieKV, 1, NAccPlus, R).

% ho finito i monomi da calcolare
polyval([], _, _, R, R) :- !.


%%	create_pairs_KV(VariablesValue, Variables, App, R)
%
%	vero quando R è una lista di coppie chiave/valore dove le chiavi
%	sono i simboli contenuti nella lista Variables e i valori sono
%	gli elementi della lista VariablesValue

% ho ancora coppie da creare
create_pairs_KV([X | Xs],[Y | Ys], App, R) :-
	!,
	create_pairs_KV(Xs, Ys, [(Y, X) | App], R).

% ho terminato le liste
create_pairs_KV(_, [], App, R) :-
	!,
	reverse(App, R).


%%	variableval(VarPower, CoppieKV, AccTimes, Result)
%
%	predicato ausiliario di polyval/5
%
%	vero quando Result è il risultato della moltiplicazione di tutte
%	le variabili contenute in VarPower e sostituite con il loro
%	corrispettivo valore estratto dalla lista CoppieKV

% ho ancora variabili da calcolare
variableval([v(P, S) | VPs], CoppieKV, AccTimes, R) :-
	member((S, V), CoppieKV),
	!,
	NAccTimes is (AccTimes * (V^P)),
	variableval(VPs, CoppieKV, NAccTimes, R).

% ho terminato le variabili da calcolare
variableval([], _, R, R) :- !.


%%	pprint_polynomial(Polynomial)
%
%	vero dopo aver stampato sullo "standard output" una
%	rappresentazione tradizionale di Polynomial

% input in forma m
pprint_polynomial(m(C, TD, VP)) :-
	!,
	pprint_polynomial(poly([m(C, TD, VP)])).

% polinomio non nullo con coefficente del primo monomio positivo uguale
% a 1 con variabili
pprint_polynomial(poly(Monomials)) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, [m(C, _, VP) | Ms]),
	C = 1,
	VP \= [],
	!,
	nl,
	pprint_variables(VP),
	pprint_monomials(Ms),
	nl, nl.


% polinomio non nullo con coefficente del primo monomio negativo uguale
% a 1 con variabili
pprint_polynomial(poly(Monomials)) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, [m(C, _, VP) | Ms]),
	C = -1,
	VP \= [],
	!,
	nl,
	write('- '),
	pprint_variables(VP),
	pprint_monomials(Ms),
	nl, nl.

% polinomio non nullo con coefficente del primo monomio positivo diverso
% da 1 con variabili
pprint_polynomial(poly(Monomials)) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, [m(C, _, VP) | Ms]),
	C >= 0,
	C \= 1,
	VP \= [],
	!,
	nl,
	write(C),
	write(' * '),
	pprint_variables(VP),
	pprint_monomials(Ms),
	nl, nl.

% polinomio non nullo con coefficente del primo monomio negativo diverso
% da -1 con variabili
pprint_polynomial(poly(Monomials)) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, [m(C, _, VP) | Ms]),
	C < 0,
	C \= -1,
	VP \= [],
	!,
	nl,
	write(C),
	write(' * '),
	pprint_variables(VP),
	pprint_monomials(Ms),
	nl, nl.

% polinomio non nullo con primo monomio senza variabili
pprint_polynomial(poly(Monomials)) :-
	is_polynomial(poly(Monomials)),
	semplify_polynomial(Monomials, [m(C, _, []) | Ms]),
	!,
	nl,
	write(C),
	pprint_monomials(Ms),
	nl, nl.

% polinomio nullo
pprint_polynomial(poly(Ms)) :-
	is_polynomial(poly(Ms)),
	semplify_polynomial(Ms, []),
	!,
	nl,
	write(0),
	nl, nl.

% polinomio costante
pprint_polynomial(poly(Ms)) :-
	is_polynomial(poly(Ms)),
	semplify_polynomial(Ms, [m(C, _, [])]),
	!,
	nl,
	write(C),
	nl, nl.


%%	pprint_monomials(Monomials)
%
%	predicato ausiliario di pprint_polynomial/1

% ho terminato i monomi
pprint_monomials([]) :- !.

% monomio senza variabili con coefficente positivo
pprint_monomials([m(C, _, []) | Ms]) :-
	C >= 0,
	!,
	write(' + '),
	write(C),
	pprint_monomials(Ms).

% monomio senza variabili con coefficente negativo
pprint_monomials([m(C, _, []) | Ms]) :-
	C < 0,
	!,
	C1 is C * (-1),
	write(' - '),
	write(C1),
	pprint_monomials(Ms).


% monomio con coefficente positivo uguale a 1
pprint_monomials([m(C, _, VP) | Ms]) :-
	C is 1,
	!,
	write(' + '),
	pprint_variables(VP),
	pprint_monomials(Ms).

% monomio con coefficente negativo uguale a 1
pprint_monomials([m(C, _, VP) | Ms]) :-
	C is -1,
	!,
	write(' - '),
	pprint_variables(VP),
	pprint_monomials(Ms).


% monomio con coefficente positivo diverso da 1
pprint_monomials([m(C, _, VP) | Ms]) :-
	C >= 0,
	C \= 1,
	!,
	write(' + '),
	write(C),
	write(' * '),
	pprint_variables(VP),
	pprint_monomials(Ms).

% monomio con coefficente negativo diverso da -1
pprint_monomials([m(C, _, VP) | Ms]) :-
	C < 0,
	C \= -1,
	!,
	C1 is C * (-1),
	write(' - '),
	write(C1),
	write(' * '),
	pprint_variables(VP),
	pprint_monomials(Ms).


%%	pprint_variables(VarPower)
%
%	predicato ausiliario di pprint_monomials
%
%	vero dopo aver stampato sullo "standard output" una
%	rappresentazione tradizionale delle variabili che compaiono in
%	VarPower

% nessuna variabile
pprint_variables([]) :- !.

% ultima variabile con esponente maggiore di 1
pprint_variables([v(P, S) | []]) :-
	P > 1,
	!,
	write(S),
	write('^'),
	write(P).

% ultima variabile con esponente uguale a 1
pprint_variables([v(P, S) | []]) :-
	P = 1,
	!,
	write(S).


% ho più di una variabile con esponente maggiore di 1
pprint_variables([v(P, S) | VPs]) :-
	P > 1,
	!,
	write(S),
	write('^'),
	write(P),
	write(' * '),
	pprint_variables(VPs).

% ho più di una variabile con esponente uguale a 1
pprint_variables([v(P, S) | VPs]) :-
	P = 1,
	!,
	write(S),
	write(' * '),
	pprint_variables(VPs).

%%% end of file polimomi.pl







































