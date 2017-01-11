/*
?- consult('mvpoli.pl').
?- load_test_files([]), run_tests.
*/
:- begin_tests(mvpoli).
%------------------------------------------------------------------------------
test(is_varpower_1) :- mvpoli:is_varpower(v(1,x)).
test(is_varpower_2) :- not(mvpoli:is_varpower(v(x,1))).
test(is_varpower_3) :- not(mvpoli:is_varpower(v(-1,x))).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(is_monomial_1) :- mvpoli:is_monomial(m(4, 1, [v(1, x)])).
test(is_monomial_2) :- mvpoli:is_monomial(m(1, 1, [v(1, x)])).
test(is_monomial_3) :- mvpoli:is_monomial(m(1, 0, [])).
test(is_monomial_4) :- not(mvpoli:is_monomial(poly([m(1, 0, [])]))).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(is_polynomial_1) :- not(mvpoli:is_polynomial(m(4, 1, [v(1, x)]))).
test(is_polynomial_2) :- mvpoli:is_polynomial(poly([m(1, 0, [])])).
test(is_polynomial_3) :- mvpoli:is_polynomial(poly([m(4, 2, [v(1, x), v(1, y)]), m(1, 2, [v(2, x)]), m(5, 2, [v(2, y)])])).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(coefficients_1) :- mvpoli:coefficients(poly([m(42, 0, [])]), [42]).
test(coefficients_2) :-	mvpoli:coefficients(poly([m(3, 1, [v(1, x)]), m(42, 0, [])]), [42, 3]).
test(coefficients_3) :-	mvpoli:coefficients(poly([m(3, 1, [v(1, x)]), m(-4, 1, [v(1, x)]), m(1, 0, [])]), [1, -1]).
test(coefficients_4) :-	mvpoli:coefficients(poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])]), [1 , 3, -4]).
test(coefficients_5) :-	mvpoli:coefficients(poly([m(1, 8, [v(2, x), v(5, y), v(1, z)]), m(1, 5, [v(1, x), v(1, y), v(3, z)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]), [1, 1, 1]).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(variables_1) :- mvpoli:variables(poly([m(42, 0, [])]), []).
test(variables_2) :- mvpoli:variables(poly([m(3, 1, [v(1, x)]),m(42, 0, [])]), [x]).
test(variables_3) :- mvpoli:variables(poly([m(3, 1, [v(1, x)]), m(-4, 1, [v(1, x)]), m(1, 0, [])]), [x]).
test(variables_4) :- mvpoli:variables(poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])]), [x]).
test(variables_5) :- mvpoli:variables(poly([m(1, 8, [v(2, x), v(5, y), v(1, z)]), m(1, 5, [v(1, x), v(1, y), v(3, z)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]), [x, y, z]).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(monomials_1) :- mvpoli:monomials(poly([m(1,1,[v(1,b)]), m(1,1,[v(1,a)])]), [m(1, 1, [v(1, a)]), m(1, 1, [v(1, b)])]).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(maxdegree_1) :- mvpoli:maxdegree(y*x^3+y*x^5+x^8+y^2+z*x^5+z*y, 8).
test(maxdegree_2) :- mvpoli:maxdegree(y^128+1, 128).
test(mindegree_1) :- mvpoli:mindegree(y*x^3+y*x^5+x^8+y^2+z*x^5+z*y, 2).
test(mindegree_2) :- mvpoli:mindegree(y^128+1, 0).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(polyplus_1) :-	mvpoli:polyplus(3*x, poly([m(4, 1, [v(1, x)])]), poly([m(7, 1, [v(1, x)])])).
test(polyplus_2) :-	mvpoli:polyplus(3*x, 3*y, poly([m(3, 1, [v(1, x)]), m(3, 1, [v(1, y)])])).
test(polyplus_3) :-	mvpoli:polyplus(poly([m(3, 1, [v(1, x)]), m(4, 1, [v(1, y)])]), 3*y, poly([m(3, 1, [v(1, x)]), m(7, 1, [v(1, y)])])).
test(polyplus_4) :-	mvpoli:polyplus(3, 39, poly([m(42, 0, [])])).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(polyminus_1) :- mvpoli:polyminus(3*x, 4*x, poly([m(-1, 1, [v(1, x)])])).
test(polyminus_2) :- mvpoli:polyminus(poly([m(3, 1, [v(1, x)])]), 4*x, poly([m(-1, 1, [v(1, x)])])).
test(polyminus_3) :- mvpoli:polyminus(3*x, poly([m(4, 1, [v(1, x)])]), poly([m(-1, 1, [v(1, x)])])).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(polytimes_1) :- mvpoli:polytimes(x^3, x, poly([m(1, 4, [v(4, x)])])).
test(polytimes_2) :- mvpoli:polytimes(x^3, poly([m(1, 8, [v(8, x)])]), poly([m(1, 11, [v(11, x)])])).
test(polytimes_3) :- mvpoli:polytimes(poly([m(1, 1, [v(1, y)]), m(1, 3, [v(3, x)])]), x^5, poly([m(1, 6, [v(5, x), v(1, y)]), m(1, 8, [v(8, x)])])).
test(polytimes_4) :- mvpoli:polytimes(x^3+y, x^5+y, poly([m(1, 2, [v(2, y)]), m(1, 4, [v(3, x), v(1, y)]), m(1, 6, [v(5, x), v(1, y)]), m(1, 8, [v(8, x)])])).
test(polytimes_5) :-
	mvpoli:as_polynomial(y*x^3+y*x^5+x^8+y^2+z*x^5+z*y, P),
	mvpoli:polytimes(x^3+y+z, x^5+y, P).
test(polytimes_6) :- mvpoli:polytimes(poly([]), poly([m(1, 3, [v(3, x)])]), poly([])).
test(polytimes_7) :- mvpoli:polytimes(x^3, 0, poly([])).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(as_monomial_1) :- mvpoli:as_monomial(42, m(42, 0, [])).
test(as_monomial_2) :- mvpoli:as_monomial(x, m(1, 1, [v(1, x)])).
test(as_monomial_3) :- mvpoli:as_monomial(x^2, m(1, 2, [v(2, x)])).
test(as_monomial_4) :- mvpoli:as_monomial(x^2*y^3, m(1, 5, [v(2, x), v(3, y)])).
test(as_monomial_5) :- mvpoli:as_monomial(69*x^2*y^3, m(69, 5, [v(2, x), v(3, y)])).
test(as_monomial_6) :- mvpoli:as_monomial(69*y^3*x^2*a^3, m(69, 8, [v(3, a), v(2, x), v(3, y)])).
test(as_monomial_7) :- mvpoli:as_monomial(y^3*a^3*ab^3*a^6, m(1, 15, [v(9, a), v(3, ab), v(3, y)])).
test(as_monomial_8) :- mvpoli:as_monomial(a*b*a^2*b^2, m(1, 6, [v(3, a), v(3, b)])).
test(as_monomial_9) :- mvpoli:as_monomial(x+x+x, m(3, 1, [v(1, x)]).
test(as_monomial_10) :- mvpoli:as_monomial(x+0+x, m(2, 1, [v(1, x)]). 
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(as_polynomial_1) :- mvpoli:as_polynomial(42, poly([m(42, 0, [])])).
test(as_polynomial_2) :- mvpoli:as_polynomial(21 + 21, poly([m(42, 0, [])])).
test(as_polynomial_3) :- mvpoli:as_polynomial(21 + x + 21 + x + x, poly([m(42, 0, []), m(3, 1, [v(1, x)])])).
test(as_polynomial_4) :- mvpoli:as_polynomial(3*x - 4*x + 1, poly([m(1, 0, []), m(-1, 1, [v(1, x)])])).
test(as_polynomial_5) :- mvpoli:as_polynomial(3*x - 4*x^2 + 1, poly([m(1, 0, []), m(3, 1, [v(1, x)]), m(-4, 2, [v(2, x)])])).
test(as_polynomial_6) :- mvpoli:as_polynomial(x*y^2 + x^2*y + x^3, poly([m(1, 3, [v(1, x), v(2, y)]), m(1, 3, [v(2, x), v(1, y)]), m(1, 3, [v(3, x)])])).
test(as_polynomial_7) :- mvpoli:as_polynomial(a*c+a^2+a*b+a, poly([m(1, 1, [v(1, a)]), m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, a), v(1, c)]), m(1, 2, [v(2, a)])])).
test(as_polynomial_8) :- mvpoli:as_polynomial(3*x-3*x, poly([])).
test(as_polynomial_9) :- mvpoli:as_polynomial(3*x-y-3*x+y, poly([])).
test(as_polynomial_10) :- mvpoli:as_polynomial(3*x-y-0-3*x+y, poly([])).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
test(polyval_1) :- mvpoli:polyval(y^3, [2], 8).
test(polyval_1) :- mvpoli:polyval(x^2-y^2, [12, 12], 0).
:- end_tests(mvpoli).