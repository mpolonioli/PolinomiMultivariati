; ------------------------------------------------------------------------------
;
;				SPECIFICHE DEL PROGETTO
;
; ------------------------------------------------------------------------------
;
;	== INTRODUZIONE ==
;	Una delle prime e piu' importanti applicazioni dei calcolatori fu la
;	manipolazione simbolica di operazioni matematiche.
;
;	In particolare, i sistemi noti come Computer Algebra Systems 
;	(cfr., Mathematica, Maple, Maxima, Axiom, etc.) si preoccupano di
;	fornire funzionalità per la manipolazione di polinomi multivariati.
;
;	Lo scopo di questo progetto e' la costruzione di una libreria 
;	in Common Lisp per la manipolazione di polinomi multivariati.
;
; ------------------------------------------------------------------------------
;
;				MEMBRI DEL GRUPPO
;
; ------------------------------------------------------------------------------
;
;;;; Polonioli Michele 780783
;;;; Nani Edoardo 780755
;;;; Cantù Stefano 763529
;
; ------------------------------------------------------------------------------
;
;				CARICAMENTO DELLA LIBRERIA
;
; ------------------------------------------------------------------------------
; 
;	1 - Eseguire l'interprete LISP nella cartella in cui è presente il file
;	    "mvpoli.LISP"
;
;	2 - Eseguire la funzione: (load "mvpoli.LISP")
;
; ------------------------------------------------------------------------------
;
;				FUNZIONI
; 
; ------------------------------------------------------------------------------
;
; Nome: IS-MONOMIAL
;
; Sintassi:
;    (is-monomial <monomial>)	
;
; Semantica: 
;    Ritorna T se <monomial> è una struttura che rappresenta un monomio
;
; Esempi:
;    (is-monomial '(m 1 1 ((v 1 x)))) --> T
;
; ------------------------------------------------------------------------------
;
; Nome: IS-VARPOWER
;
; Sintassi:
;    (is-varpower <varpower>)
;
; Semantica:
;    Ritorna T se <varpower> è una struttura che rappresenta una variabile
;
; Esempi:
;    (is-varpower '(v 1 x)) --> T
;
; ------------------------------------------------------------------------------
;
; Nome: IS-POLYNOMIAL
;
; Sintassi:
;    (is-polynomial <polynomial>)
;
; Semantica:
;    Ritorna T se <polynomial> è una struttura che rappresenta un polinomio
;
; Esempi:
;    (is-polynomial '(POLY ((m 1 1 ((v 1 a))) (m 1 1 ((v 1 b)))))) --> T
;
; ------------------------------------------------------------------------------
;
; Nome: AS-MONOMIAL
;
; Sintassi: 
;    (as-monomial <espressione>)
;
; Semantica:
;    Ritorna la struttura <monomial> data dal "parsing" di <espressione>
;
; Esempi:
;    (as-monomial 42) --> (M 42 0 NIL)
;    (as-monomial '(* 42 (expt a 1))) --> (M 42 1 ((v 1 a)))
;
; ------------------------------------------------------------------------------
;
; Nome: AS-POLYNOMIAL
;
; Sintassi:
;    (as-polynomial <espressione>)
;
; Semantica:
;    Ritorna la struttura <polynomial> data dal "parsing" di <espressione>
;
; Esempi:
;    (as-polynomial '(+ 42 (* 42 (expt a 1)))))
;     		     --> (POLY ((M 42 0 NIL) (M 42 ((V 1 A)))))
;
; ------------------------------------------------------------------------------
;
; Nome: VARPOWERS
;
; Sintassi: 
;    (varpowers <monomial>)
;
; Semantica: Ritorna la lista di varpower contenuta della struttura <monomial>
;
; Esempi:
;    (varpowers '(m 1 2 ((v 1 a) (v 1 b)))) --> ((v 1 a) (v 1 b))
;
; ------------------------------------------------------------------------------
;
; Nome: VARIABLES
;
; Sintassi:
;    (variables <polynomial>)
;    (variables <monomial>)
;    (variables <espressione>)
;
; Semantica: 
;    Ritorna la lista dei simboli di variabile contenuti nella struttura 
;     <polynomial>, oppure della struttura <monomial>, oppure nella struttura 
;     <polynomial> data dal "parsing" di <espressione>
;
;    La lista dei simboli di variabile viene tornata in ordine lessicografico 
;    crescente senza ripetizioni
;
; Esempi:
;    (variables '(POLY (M 1 2 ((V 1 A) (V 1 B))))) --> (A B)
;    (variables '(M 1 2 ((V 1 A) (V 1 B)))) --> (A B) 
;    (variables '(+ (* a b)) --> (A B)
;
; ------------------------------------------------------------------------------
;
; Nome: VARS-OF
;
; Sintassi:
;    (vars-of <monomial>)
;
; Semantica:
;    Ritorna la lista dei simboli di variabile contenuti nella struttura 
;    <monomial>
;
;    La lista dei simboli di variabile viene tornata in ordine lessicografico 
;    crescente senza ripetizioni
;
; Esempi:
;    (vars-of '(M 1 2 ((V 1 A) (V 1 B)))) --> (A B)
;
; ------------------------------------------------------------------------------
;
; Nome: MONOMIAL-DEGREE
; 
; Sintassi:
;    (monomial-degree <monomial>)
;
; Semantica:
;    Ritorna il grado totale contenuto nella struttura <monomial>
;
; Esempi:
;    (monomial-degree '(M 1 2 ((V 2 A)))) --> 2
;
; ------------------------------------------------------------------------------
;
; Nome: MONOMIAL-COEFFICIENT
; 
; Sintassi:
;    (monomial-coefficient <monomial>)
;
; Semantica:
;    Ritorna il coefficiente contenuto nella struttura <monomial>
;
; Esempi:
;    (monomial-coefficient '(M 42 1 ((V 1 A)))) --> 42
;
; ------------------------------------------------------------------------------
;
; Nome: MONOMIALS
;
; Sintassi:
;    (monomials <polynomial>)
;    (monomials <monomial>)
;    (monomials <espressione>)
;
; Semantica:
;    Ritorna la lista dei monomi contenuti nella struttura <polynomial>,
;    oppure la lista contentente la struttura <monomial>,
;    oppure la lista dei monomi contenuti nell struttura <polynomial> data
;    dal "parsing" di <espressione>
;
; Esempi:
;    (monomials '(POLY (M 1 2 ((V 1 A) (V 1 B)))))
;     		 --> ((M 1 2 ((V 1 A) (V 1 B))))
;    (monomials '(M 1 2 ((V 1 A) (V 1 B)))) 
;     		--> ((M 1 2 ((V 1 A) (V 1 B))))
;    (monomials '(+ (* a b)) 
;     		--> ((M 1 2 ((V 1 A) (V 1 B))))
;
; ------------------------------------------------------------------------------
;
; Nome: COEFFICIENTS
;
; Sintassi:
;    (coefficients <polynomial>)
;    (coefficients <monomial>)
;    (coefficients <espressione>)
;
; Semantica:
;    Ritorna la lista dei coefficienti di monomio contenuti nella struttura 
;    <polynomial>,
;    oppure la lista contenente il coefficiente contenuto nella struttura 
;    <monomial>,
;    oppure la lista dei coefficienti di monomio contenuti nella struttura 
;    <polynomial> data dal "parsing" di <espressione>
;
; Esempi:
;    (coefficients '(POLY (M 42 2 ((V 1 A) (V 1 B))))) --> (42)
;    (coefficients '(M 42 2 ((V 1 A) (V 1 B)))) --> (42)
;    (coefficients '(+ (* 42 a b)) --> (42)
;
; ------------------------------------------------------------------------------
;
; Nome: MAXDEGREE
;
; Sintassi:
;    (maxdegree <polynomial>)
;    (maxdegree <monomial>)
;    (maxdegree <espressione>)
;
; Semantica:
;    Ritorna il massimo grado di monomio dei monomi contenuti nella struttura
;    <polynomial>, oppure il grado del monomio contenuto nella struttura 
;    <monomial>,
;    oppure il massimo grado di monomio dei monomi contenuti nella struttura 
;    <polynomial> data dal "parsing" di <espressione>
;
; Esempi:
;    (maxdegree '(POLY (M 42 3 ((V 2 A) (V 1 B))))) --> 2
;    (maxdegree '(M 42 3 ((V 2 A) (V 1 B)))) --> 2
;    (maxdegree '(+ (* 42 (expt a 2) b)) --> 2
;
; ------------------------------------------------------------------------------
;
; Nome: MINDEGREE
;
; Sintassi:
;    (mindegree <polynomial>)
;    (mindegree <monomial>)
;    (mindegree <espressione>)
;
; Semantica:
;    Ritorna il minimo grado di monomio dei monomi contenuti nella struttura
;    <polynomial>, oppure il grado del monomio contenuto nella struttura 
;    <monomial>,
;    oppure il minimo grado di monomio dei monomi contenuti nella struttura 
;    <polynomial> data dal "parsing" di <espressione>
;
; Esempi:
;    (mindegree '(POLY (M 42 3 ((V 2 A) (V 1 B))))) --> 1
;    (mindegree '(M 42 3 ((V 2 A) (V 1 B)))) --> 1
;    (mindegree '(+ (* 42 (expt a 2) b)) --> 1
;
; ------------------------------------------------------------------------------
;
; Nome: POLYPLUS
;
; Sintassi:
;    (polyplus <polynomial> <polynomial>)
;    (polyplus <monomial> <monomial>)
;    (polyplus <espressione> <espressione>)
;    (polyplus <polynomial> <monomial>)
;    (polyplus <monomial> <polynomial>)
;    (polyplus <monomial> <espressione>)
;    (polyplus <espressione> <monomial>)
;    (polyplus <polynomial> <espressione>)
;    (polyplus <espressione> <polynomial>)
;
; Semantica:
;    Ritorna la struttura <polynomial> risultante dalla somma di
;    due strutture <polynomial>,
;    due strutture <monomial>,
;    due strutture <polynomial> ottenute dal "parsing" di un <espressione>,
;    oppure tutte le possibili combinazioni di questi 3 elementi
;
; Esempi:
;    (polyplus '(POLY ((M 1 1 ((V 1 A))))) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(M 1 1 ((V 1 A))) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(+ (* a)) '(+ (* b)))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(POLY ((M 1 1 ((V 1 A))))) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(M 1 1 ((V 1 A))) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(M 1 1 ((V 1 A))) '(+ (* b)))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(+ (* a)) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(POLY ((M 1 1 ((V 1 A))))) '(+ (* b)))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
;    (polyplus '(+ (* a)) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M 1 1 ((V 1 B)))))
;
; ------------------------------------------------------------------------------
;
; Nome: POLYMINUS
;
; Sintassi:
;    (polyminus <polynomial> <polynomial>)
;    (polyminus <monomial> <monomial>)
;    (polyminus <espressione> <espressione>)
;    (polyminus <polynomial> <monomial>)
;    (polyminus <monomial> <polynomial>)
;    (polyminus <monomial> <espressione>)
;    (polyminus <espressione> <monomial>)
;    (polyminus <polynomial> <espressione>)
;    (polyminus <espressione> <polynomial>)
;
; Semantica:
;    Ritorna la struttura <polynomial> risultante dalla differenza di
;    due strutture <polynomial>,
;    due strutture <monomial>,
;    due strutture <polynomial> ottenute dal "parsing" di un <espressione>,
;    oppure tutte le possibili combinazioni di questi 3 elementi
;
; Esempi:
;    (polyminus '(POLY ((M 1 1 ((V 1 A))))) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(M 1 1 ((V 1 A))) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(+ (* a)) '(+ (* b)))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(POLY ((M 1 1 ((V 1 A))))) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(M 1 1 ((V 1 A))) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(M 1 1 ((V 1 A))) '(+ (* b)))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(+ (* a)) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(POLY ((M 1 1 ((V 1 A))))) '(+ (* b)))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
;    (polyminus '(+ (* a)) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 1 ((V 1 A))) (M -1 1 ((V 1 B)))))
;
; ------------------------------------------------------------------------------
;
; Nome: POLYTIMES
;
; Sintassi:
;    (polytimes <polynomial> <polynomial>)
;    (polytimes <monomial> <monomial>)
;    (polytimes <espressione> <espressione>)
;    (polytimes <polynomial> <monomial>)
;    (polytimes <monomial> <polynomial>)
;    (polytimes <monomial> <espressione>)
;    (polytimes <espressione> <monomial>)
;    (polytimes <polynomial> <espressione>)
;    (polytimes <espressione> <polynomial>)
;
; Semantica:
;    Ritorna la struttura <polynomial> risultante dal prodotto di
;    due strutture <polynomial>,
;    due strutture <monomial>,
;    due strutture <polynomial> ottenute dal "parsing" di un <espressione>,
;    oppure tutte le possibili combinazioni di questi 3 elementi
;
; Esempi:
;    (polytimes '(POLY ((M 1 1 ((V 1 A))))) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(M 1 1 ((V 1 A))) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(+ (* a)) '(+ (* b)))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(POLY ((M 1 1 ((V 1 A))))) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(M 1 1 ((V 1 A))) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(M 1 1 ((V 1 A))) '(+ (* b)))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(+ (* a)) '(M 1 1 ((V 1 B))))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(POLY ((M 1 1 ((V 1 A))))) '(+ (* b)))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
;    (polytimes '(+ (* a)) '(POLY ((M 1 1 ((V 1 B))))))
;	--> (POLY ((M 1 2 ((V 1 A) (V 1 B)))))
;
; ------------------------------------------------------------------------------
;
; Nome: PPRINT-POLYNOMIAL
;
; Sintassi:
;    (pprint-polynomial <monomial>)
;    (pprint-polynomial <polynomial>)
; 
; Semantica:
;    Ritorna NIL dopo aver stampado una rappresentazione tradizionale
;    della struttura <polynomial> o della struttura <monomial>
;
; Esempi:
;    (pprint-polynomial '(POLY ((M 42 1 ((V 1 A))) (M -42 1 ((V 1 B))))))
;	--> +42*A -42*B
;	    NIL
;
;    (pprint-polynomial '(M 42 1 ((V 1 A))))
;	--> +42*A
;	    NIL
;
; ------------------------------------------------------------------------------
;
;				NOTE
;
; ------------------------------------------------------------------------------
;
;    Sono state specificate soltanto le funzioni pensate per essere utilizzate
;    anche all'esterno della libreria.
;    
;    La libreria contiene anche altre funzioni necessarie per il funzionamento
;    della stessa, ma che non sono pensate per essere utilizzate al di fuori.
;
; ------------------------------------------------------------------------------
;
;                               EOF: END-OF-FILE                               
;
; ------------------------------------------------------------------------------
