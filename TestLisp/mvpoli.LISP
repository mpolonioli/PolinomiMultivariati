;;;; <780783> <Polonioli> <Michele>
;;;; <780755> <Nani> <Edoardo>
;;;; <763529> <Cantù> <Stefano>


;;;; -*- Mode: LISP -*-

;; polinomi.LISP


; Monomio : (m coefficient total-degree vars-n-powers)
; Variabile: (v power var-symbol)
; Polinomio: (poly monomials)


; T se m e' un monomio
(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-total-degree m))
             (vps (monomial-vars-and-powers m))
             )
         (and (integerp mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))


; estrae il coefficente da un monomio
(defun m-coefficient (m)
  (second m))


; estrae il grado totale da un monomio
(defun monomial-total-degree (m)
  (third m))


; estrae la lista di varpowers da un monomio
(defun monomial-vars-and-powers (m)
  (fourth m))


; T se vp e' una variabile
(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp))
             )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))


; estrae la potenza da una variabile
(defun varpower-power (vp)
  (second vp))


; estrae il simbolo di una variabile
(defun varpower-symbol (vp)
  (third vp))


; T se p e' un polinomio
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (poly-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


; estrae la lista dei monomi di p
(defun poly-monomials (p)
  (second p))


; restituisce un monomio parsato, semplificato e ordinato
(defun as-monomial (input)
      (cond ((numberp input)
             (list 'm (eval input) 0 ()))

            ((symbolp input)
             (list 'm 1 1 (list 'v 1 input)))

            ((and (listp input) (eq '* (first input)))
             (semplify-monomial (parse-monomial input)))

            (T 
             (error "Errore: input non valido per il parsing del monomio"))
            ))


; si occupa del parsing di un monomio
(defun parse-monomial (input)
  (cond ((symbolp (second input))
         (let ((vps (parse-vars-and-powers (cdr input) ())))
           (list 'm 1 (calc-total-degree vps 0)  vps)
           ))

         ((and (listp (second input)) 
               (symbolp (second (second input))))
         (let ((vps (parse-vars-and-powers (cdr input) ())))
           (list 'm 1 (calc-total-degree vps 0) vps)
           ))

         ((eq (eval (second input)) 0)
          (list 'm 0 0 nil)
          )

         (T 
          (let ((c (eval (second input)))
                (vps (parse-vars-and-powers (cdr (cdr input)) ())))
            (list 'm c (calc-total-degree vps 0) vps)
            ))
         ))


; si occupa del parsing delle variabili
(defun parse-vars-and-powers (input app)
  (let ((var (first input)))
    (cond ((eq var ())
           app)

          ((symbolp var)
           (parse-vars-and-powers 
            (cdr input) 
            (append app (list (list 'v 1 var)))
            ))

          ((and (listp var)
                (eq 'expt (first var))
                (integerp (third var))
                (>= (third var) 0)
                (symbolp (second var))
                )
           (parse-vars-and-powers 
            (cdr input) 
            (append app (list (list 'v (third var) (second var))))
            ))

          (T 
           (error "input non valido per il parsing delle variabili")
          ))
  ))


; calcola la somma dei gradi delle variabili contenute in vps
(defun calc-total-degree (vps app)
  (cond ((eq vps ()) 
         app)

        (T 
         (calc-total-degree (cdr vps) (+ app (varpower-power (car vps)))))
        )
  )


; data una struttura Monomial torna la struttura Monomial seplificata
(defun semplify-monomial (m)
  (cond ((is-monomial m)
         (list 'm 
               (m-coefficient m) 
               (monomial-total-degree m)
               (semplify-vars-and-powers (monomial-vars-and-powers m))))
        
        ((eq m nil)
         m
         )
        
        (T
         (error "input non valido per la semplifica del monomio")))
  )

; data una lista di strutture Monomial torna la lista di strutture Monomial 
; semplificate
(defun semplify-monomials (ms app)
  (let ((m (first ms)))
    (if (eq m nil)
        app
      (semplify-monomials (cdr ms) (append app (list (semplify-monomial m))))
      )
    ))


; ritrona la lista di variabili contenute in vars semplificate
(defun semplify-vars-and-powers (vars)
  (cond ((every #'is-varpower vars)
         (semplify-vars-and-powers-r (sort-vars-and-powers vars) ()))

        (T
         (error "input non valido per la semplifica delle variabili")))
  )


; esegue la semplifica delle variabili in maniera ricorsiva
(defun semplify-vars-and-powers-r (vars app)

(let ((vp1 (first vars))
      (vp2 (second vars))
      (vps (cdr (cdr vars))))

  (cond ((and (is-varpower vp1)
              (is-varpower vp2)
              (eq (varpower-symbol vp1) (varpower-symbol vp2)))
         (let ((newvp (list 
                       'v 
                       (+ (varpower-power vp1) (varpower-power vp2)) 
                       (varpower-symbol vp1))))
           (semplify-vars-and-powers-r (append (list newvp) vps)  app)))

        ((and (is-varpower vp1)
              (is-varpower vp2)
              (not (eq (varpower-symbol vp1) (varpower-symbol vp2))))
         (semplify-vars-and-powers-r 
          (append (list vp2) vps) 
          (append (list vp1) app)))

        ((and (is-varpower vp1) 
              (eq vp2 ()))        
         (semplify-vars-and-powers-r vps (append (list vp1) app)))

        ((eq vp1 ())
         (zero-eater-v (reverse app) ()))

        (T
         (error "durante la semplifica ricorsiva delle variabili")))
  ))


; ritorna le variabili contenuti in vps senza quelle con esponente nullo
(defun zero-eater-v (vps app)

  (let ((vp (first vps)))
    (cond ((and (is-varpower vp)
                (= (varpower-power vp) 0))
           (zero-eater-v (cdr vps) app))
          
          ((and (is-varpower vp)
                (not (= (varpower-power vp) 0)))
           (zero-eater-v (cdr vps) (append (list vp) app)))
          
          ((eq vp ())
           (reverse app))
          
          (T 
           (error "durante l'eliminazione delle variabili con esponente zero")))
    ))


; 1   se vp1 = vp2
; 0   se vp1 < vp2
; nil se vp1 > vp2
(defun vp<= (vp1 vp2)
  (if (and (is-varpower vp1)
           (is-varpower vp2))
      (let ((vp1-symbol (varpower-symbol vp1))
            (vp2-symbol (varpower-symbol vp2)))
        (cond ((eq (string<=  vp1-symbol vp2-symbol) 1)
               1)
              ((eq (string<= vp1-symbol vp2-symbol) nil)
              nil)
              ((eq (string<= vp1-symbol vp2-symbol) 0)
               0)
              (T
               (error "durante il confronto tra variabili"))
              ))
    (error "input non valido per il confronto tra variabili"))
  )


; riordina le variabili secondo le specifiche
(defun sort-vars-and-powers (vps)
  (sort (copy-seq vps) #'vp<=)
  )


; restituisce un polinomio parsato, semplificato e riordinato
(defun as-polynomial (input)
  (cond ((and (listp input)
              (eq (first input) '+))
         (semplify-polynomial (parse-polynomial (cdr input) ())))
        
        (T
         (semplify-polynomial (list 'POLY (list (as-monomial input)))))
        ))


; si occupa del parsing del polinomio
(defun parse-polynomial (ms app)
  (cond ((eq ms ())
         (list 'poly app))
        
        (T
         (parse-polynomial (cdr ms) 
                           (append (list (as-monomial (first ms))) app)))
        ))


; data una struttura Poly torna la struttura Poly semplificata
(defun semplify-polynomial (poly)

  (if (is-polynomial poly)

      (let ((ms (sort-monomials 
                 (zero-eater-m 
                  (semplify-monomials (poly-monomials poly) ()) 
                  ()))))
        (list 'POLY 
              (zero-eater-m (semplify-polynomial-r (first ms) (cdr ms) ()) ()))
        )
    
    (error "input non valido per la semplifica del polinomio")
    ))

; data una lista di strutture Monomial torna la lista di strutture Monomial
; semplificata
(defun semplify-polynomial-r (m1 ms app)

  (let ((m2 (first ms))
        (vps1 (monomial-vars-and-powers m1))
        (vps2 (monomial-vars-and-powers (first ms)))
        (c1 (m-coefficient m1))
        (c2 (m-coefficient (first ms)))
        (td (monomial-total-degree m1)))
    
    (cond ((eq m1 nil)
           app)
          
          ((eq m2 nil)
           (append app (list m1)))
          
          ((and (equal vps1 vps2)
                (not (eq m1 nil))
                (not (eq m2 nil)))
           (semplify-polynomial-r (list 'm (+ c1 c2) td vps1) (cdr ms) app)
           )

          ((and (not (equal vps1 vps2))
                (not (eq m1 nil))
                (not (eq m2 nil)))
           (semplify-polynomial-r m2 (cdr ms) (append app (list m1))))
          
          (T
           (error "durante la semplifica dei monomi"))
          ))
  )


; elimina monomi con coefficente 0 da una lista di monomi
(defun zero-eater-m (ms app)
  
  (let ((m (first ms)))
    
    (cond ((eq m nil)
          app)
    
          ((= (m-coefficient m) 0)
           (zero-eater-m (cdr ms) app))

          ((not (= (m-coefficient m) 0))
           (zero-eater-m (cdr ms) (append app (list m))))
          
          (T
           (error "durante l'eliminazione dei monomi con coefficente nullo"))
          ))
  )

; vps1 e vps2 devono avere grado totale identico
; 1   se vps1 = vps2
; 0   se vps1 < vps2
; nil se vps1 > vps2
(defun vars-and-powers<= (vps1 vps2)

  (let ((vp1 (first vps1))
        (vp2 (first vps2)))
    
    (cond ((and (eq vp1 nil)
                (not (eq vp2 nil)))
           0)
          
          ((and (not (eq vp1 nil))
                (eq vp2 nil))
           nil)

          ((and (eq vp1 nil)
                (eq vp2 nil))
           1)

          ((eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) 0)
           0)

          ((eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) nil)
           nil)
          
          ((and (eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) 1)
                (< (varpower-power vp1) (varpower-power vp2)))
           0)
          
          ((and (eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) 1)
                (> (varpower-power vp1) (varpower-power vp2)))
           nil)
          
          ((and (eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) 1)
                (= (varpower-power vp1) (varpower-power vp2)))
           (vars-and-powers<= (cdr vps1) (cdr vps2)))

          
          (T 
           (error "durante il confronto tra liste di variabili"))
          ))
    )       


; m1 e m2 devono avere variabili ordinate
; 1   se m1 = m2
; 0   se m1 < m2
; nil se m1 > m2
(defun ms<= (m1 m2)
  
  (if (and (is-monomial m1)
           (is-monomial m2))
      
      (let ((td1 (monomial-total-degree m1))
            (td2 (monomial-total-degree m2))
            (vps1 (monomial-vars-and-powers m1))
            (vps2 (monomial-vars-and-powers m2)))
        
        (cond ((< td1 td2)
               0)

              ((> td1 td2)
               nil)
              
              ((= td1 td2)
               (vars-and-powers<= vps1 vps2))
              
              (T
               (error "Durante il confronto tra monomi"))
              ))

    (error "Input non valido per il confronto tra monomi")
    ))


; riordina una lista di monomi secondo le specifiche
(defun sort-monomials (monomials)
  (sort (copy-seq monomials) #'ms<=)
  )


; data una lista di varpower torna la lista dei simboli di variabile in essa
; senza ripetizioni e ordinata in ordine lessicografico crescente
(defun vars-and-powers-variables (vps app)
  
  (let ((vp (first vps)))
    
    (cond ((is-varpower vp)
           (vars-and-powers-variables (cdr vps)
                                      (append app (list (varpower-symbol vp)))))
          
          ((eq vp nil)
           (sort (copy-seq (remove-duplicates (copy-seq app))) #'string<=))
          
          (T
           (error "durante l'estrazione dei simboli di variabile da vps"))
          ))
  )


; data una lista di monomi torna la lista dei simboli di variabile
; senza ripetizioni e ordinata in ordine lessicografico crescente
(defun monomials-variables (ms app)
  
  (let ((m (first ms)))
    
    (cond ((eq m nil)
          (sort (copy-seq (remove-duplicates (copy-seq app))) #'string<=))
          
          ((is-monomial m)
           (monomials-variables (cdr ms) 
                                (append app (vars-and-powers-variables 
                                             (monomial-vars-and-powers m) ())))
           )

          (T
           (error "durante l'estrazione dei simboli di varabile da ms"))
          
          ))
  )    
    

; dato un polinomio torna la lista dei simboli di variabile contenuti in esso
; senza ripetizioni in ordine lessicografico crescente
(defun variables (poly)
  
  (cond ((is-monomial poly)
         (vars-and-powers-variables (monomial-vars-and-powers poly) ()))
        
        ((is-polynomial poly)
         (monomials-variables (poly-monomials poly) ()))
        
        (T
         (monomials-variables (poly-monomials (as-polynomial poly)) ()))
        ))


; data una struttura Monomial, ritorna la lista di varpowers Vp-list
(defun varpowers (monomial)
  (if (is-monomial monomial)
      (monomial-vars-and-powers (semplify-monomial monomial))
    (varpowers (as-monomial monomial))
    ))


; data una struttura Monomial, ritorna la lista di variabili Variables
(defun vars-of (monomial)

  (if (is-monomial monomial)
      (monomials-variables (list monomial) ())
    (vars-of (as-monomial monomial)))
  )


; data una struttura Monomial, ritorna il suo grado totale TotalDegree
(defun monomial-degree (monomial)

  (if (is-monomial monomial)
      (monomial-total-degree monomial)
    (monomial-degree (as-monomial monomial)))
  )


; ritorna la lista ordinata dei monomi che appaiono in poly 
(defun monomials (poly)

  (cond ((is-monomial poly)
         (list (semplify-monomial poly)))
        
        ((is-polynomial poly)
         (poly-monomials (semplify-polynomial poly)))
        
        (T
         (poly-monomials (as-polynomial poly)))
        ))


; estrae il coefficente da un monomio
(defun monomial-coefficient (m)
  (cond ((and (is-monomial m)
              (eq (m-coefficient m) nil))
         0)

        ((and (is-monomial m)
              (not (eq (m-coefficient m) nil)))
         (m-coefficient m))
        
        (T
         (monomial-coefficient (as-monomial m)))
    ))


; ritorna una lista dei coefficienti da una lista di monomi
(defun monomials-coefficients (monomial-list app)
  (let ((monomial (first monomial-list)))
    (cond ((eq monomial nil)
          app)
          
          (T
           (monomials-coefficients (cdr monomial-list) 
                                   (append 
                                    app 
                                    (list (monomial-coefficient monomial)))))
          ))
  )


; ritorna una lista Cofficients dei coffcienti di Poly
(defun coefficients (poly)
  (cond ((is-monomial poly)
         (list (monomial-coefficent (semplify-monomial poly))))
        
        ((and (is-polynomial poly)
              (eq (monomials poly) nil))
         (list 0))

        ((and (is-polynomial poly)
              (not (eq (monomials poly) nil)))
         (monomials-coefficients (monomials poly) ()))
        
        (T
         (coefficients (as-polynomial poly)))
        ))


; ritorna il massimo grado dei monomi che compaiono in poly
(defun maxdegree (poly)
  (cond ((is-monomial poly)
         (monomial-degree (semplify-monomial poly)))

        ((is-polynomial poly)
         (monomial-degree (first 
                           (reverse 
                            (monomials (semplify-polynomial poly))))))

        (T
         (monomial-degree (first (reverse (monomials (as-polynomial poly))))))
        ))


; ritorna il minimo grado dei monomi che compaiono in poly
(defun mindegree (poly)
  (cond ((is-monomial poly)
         (monomial-degree (semplify-monomial poly)))

        ((is-polynomial poly)
         (monomial-degree (first (monomials (semplify-polynomial poly)))))
        
        (T
         (monomial-degree (first (monomials (as-polynomial poly)))))
        ))


; produce il polinomio somma di poly1 e poly2
(defun polyplus (poly1 poly2)
  (cond ((and (is-monomial poly1)
              (is-monomial poly2))
         (semplify-polynomial (list 'poly (list poly1 poly2))))

        ((and (is-monomial poly1)
              (is-polynomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append (list poly1) 
                                            (monomials poly2)))))
       
        ((and (is-polynomial poly1)
              (is-monomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append (monomials poly1) 
                                            (list poly2)))))
        
        ((and (is-polynomial poly1)
              (is-polynomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append (monomials poly1) 
                                            (monomials poly2)))))
        
        ((and (is-monomial poly1)
              (not (is-polynomial poly2))
              (not (is-monomial poly2)))
         (semplify-polynomial (list 'poly (append 
                                           (list poly1) 
                                           (monomials (as-polynomial poly2))))))

        ((and (not (is-monomial poly1))
              (not (is-polynomial poly1))
              (is-monomial poly2))
         (semplify-polynomial (list 'poly (append
                                           (monomials (as-polynomial poly1))
                                           (list poly2)))))

        ((and (is-polynomial poly1)
              (not (is-monomial poly2))
              (not (is-polynomial poly2)))
         (semplify-polynomial (list 'poly (append
                                           (monomials poly1)
                                           (monomials (as-polynomial poly2))))))

        ((and (not (is-monomial poly1))
             (not (is-polynomial poly1))
             (is-polynomial poly2))
        (semplify-polynomial (list 'poly (append
                                          (monomials (as-polynomial poly1))
                                          (monomials poly2)))))

        (T
         (semplify-polynomial (list 'poly (append 
                                           (monomials (as-polynomial poly1)) 
                                           (monomials (as-polynomial poly2))))))
        ))


; data una lista di monomi ritorna una lista di monomi con segno 
; dei coefficienti invertito
(defun invert-sign (monomials app)
  (let ((m (first monomials)))
    (cond ((eq m nil)
           app)

          (T
           (invert-sign (cdr monomials) (append 
                                         (list 
                                          (list 'M 
                                                (* -1 (m-coefficient m))
                                                (monomial-total-degree m)
                                                (monomial-vars-and-powers m))) 
                                         app)))
          ))
  )


; produce il polinomio differenza di poly1 e poly2
(defun polyminus (poly1 poly2)
  (cond ((and (is-monomial poly1)
              (is-monomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (list poly1)  
                                     (invert-sign (list poly2) ())))))
        
        ((and (is-monomial poly1)
              (is-polynomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (list poly1) 
                                     (invert-sign (monomials poly2) ())))))
        
        ((and (is-polynomial poly1)
              (is-monomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (monomials poly1) 
                                     (invert-sign (list poly2) ())))))
        
        ((and (is-polynomial poly1)
              (is-polynomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (monomials poly1)
                                     (invert-sign (monomials poly2) ())))))

        ((and (is-monomial poly1)
              (not (is-polynomial poly2))
              (not (is-monomial poly2)))
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (list poly1) 
                                     (invert-sign 
                                      (monomials (as-polynomial poly2)) ())))))

        ((and (not (is-monomial poly1))
              (not (is-polynomial poly1))
              (is-monomial poly2))
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (monomials (as-polynomial poly1))
                                     (invert-sign (list poly2) ())))))

        ((and (is-polynomial poly1)
              (not (is-monomial poly2))
              (not (is-polynomial poly2)))
         (semplify-polynomial (list 'poly 
                                    (append
                                     (monomials poly1)
                                     (invert-sign 
                                      (monomials (as-polynomial poly2) ()))))))

        ((and (not (is-monomial poly1))
             (not (is-polynomial poly1))
             (is-polynomial poly2))
        (semplify-polynomial (list 'poly 
                                   (append
                                    (monomials (as-polynomial poly1))
                                    (invert-sign (monomials poly2) ())))))
        
        (T
         (semplify-polynomial (list 'poly 
                                    (append 
                                     (monomials (as-polynomial poly1)) 
                                     (invert-sign 
                                      (monomials (as-polynomial poly2)) 
                                      ())))))
        ))


; ritorna il polinomio risultante dalla moltiplicazione di poly1 e poly2
(defun polytimes (poly1 poly2)
  (cond ((and (is-monomial poly1)
              (is-monomial poly2))
         (semplify-polynomial (list 
                               'POLY 
                               (polytimes-calc 
                                (list poly1) 
                                (list poly2) 
                                (list poly2) 
                                ()))))
        
        ((and (is-monomial poly1)
              (is-polynomial poly2))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc 
                                (list poly1) 
                                (poly-monomials poly2)
                                (poly-monomials poly2)
                                ()))))
        
        ((and (is-polynomial poly1)
              (is-monomial poly2))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (poly-monomials poly1)
                                (list poly2)
                                (list poly2)
                                ()))))

        ((and (is-polynomial poly1)
              (is-polynomial poly2))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (poly-monomials poly1)
                                (poly-monomials poly2)
                                (poly-monomials poly2)
                                ()))))

        ((and (is-polynomial poly1)
              (not (is-monomial poly2))
              (not (is-polynomial poly2)))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (poly-monomials poly1)
                                (poly-monomials (as-polynomial poly2))
                                (poly-monomials (as-polynomial poly2))
                                ()))))

        ((and (is-monomial poly1)
              (not (is-monomial poly2))
              (not (is-polynomial poly2)))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (list poly1)
                                (poly-monomials (as-polynomial poly2))
                                (poly-monomials (as-polynomial poly2))
                                ()))))

        ((and (not (is-monomial poly1))
              (not (is-polynomial poly1))
              (is-polynomial poly2))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (poly-monomials (as-polynomial poly1))
                                (poly-monomials poly2)
                                (poly-monomials poly2)
                                ()))))

        ((and (not (is-monomial poly1))
              (not (is-polynomial poly1))
              (is-monomial poly2))
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (poly-monomials (as-polynomial poly1))
                                (list poly2)
                                (list poly2)
                                ()))))
        
        (T
         (semplify-polynomial (list
                               'POLY
                               (polytimes-calc
                                (poly-monomials (as-polynomial poly1))
                                (poly-monomials (as-polynomial poly2))
                                (poly-monomials (as-polynomial poly2))
                                ()))))
        ))


; esegue il calcolo della moltiplicazione tra 2 liste di monomi
(defun polytimes-calc (ms1 ms2 ms2-copy app)
  (let ((c1 (m-coefficient (first ms1)))
        (c2 (m-coefficient (first ms2)))
        (td1 (monomial-total-degree (first ms1)))
        (td2 (monomial-total-degree (first ms2)))
        (vps1 (monomial-vars-and-powers (first ms1)))
        (vps2 (monomial-vars-and-powers (first ms2))))
    
    (cond ((eq ms1 ())
           app)
          
          ((eq ms2 ())
           (polytimes-calc (cdr ms1) ms2-copy ms2-copy app))
          
          (T
           (polytimes-calc
            ms1
            (cdr ms2)
            ms2-copy
            (append app (list (semplify-monomial (list
                                                  'M
                                                  (* c1 c2)
                                                  (+ td1 td2)
                                                  (append vps1 vps2)))))))
          ))
  )


; data una lista di simboli di variabile e una lista di valori produce 
; la lista delle coppie simbolo-valore
(defun create-pairs-kv (variables values)
  (cond ((eq variables nil)
        nil)
        
        ((and (not (eq variables nil))
              (eq values nil))
         (error "valori mancanti per il binding chiave-valore"))

        (T
         (cons (list (first variables) (first values)) 
               (create-pairs-kv (cdr variables) (cdr values))))
        ))


; esegue il calcolo del punto n-dimensionale di una lista di monomi
(defun polyval-calc (ms pairs-kv)
  (cond ((eq ms nil)
        0)
        
        (T
         (+ (* (m-coefficient (first ms)) 
               (calc-vps-value 
                (monomial-vars-and-powers (first ms)) 
                pairs-kv)) 
            (polyval-calc (cdr ms) pairs-kv)))
        ))


; esegue il calcolo del punto n-dimensionale di una lista di varpower
(defun calc-vps-value (vps pairs-kv)
  (cond ((eq vps nil)
        1)

        (T
         (* (expt (get-varpower-value (varpower-symbol (first vps)) pairs-kv) 
                  (varpower-power (first vps)))
            (calc-vps-value (cdr vps) pairs-kv)))
        ))


; data un simbolo e una lista simbolo-valore torna il valore associato 
; al simbolo
(defun get-varpower-value (k pairs-kv)
  (cond ((eq k (first (first pairs-kv)))
        (second (first pairs-kv)))
        
        (T
         (get-varpower-value k (cdr pairs-kv)))
        ))


; restituisce il valore Value del polinomio Polynomial, 
; nel punto n-dimensionale: rappresentato dalla lista VariableValues,
; che contiene un valore per ogni variabile ottenuta con la funzione variables
(defun polyval (poly variable-values)
  (cond ((is-monomial poly)
        (polyval-calc 
         (list poly) 
         (create-pairs-kv (variables poly) variable-values)))
        
        ((is-polynomial poly)
         (polyval-calc 
          (poly-monomials poly)
          (create-pairs-kv (variables poly) variable-values)))
        
        (T
         (let ((p (as-polynomial poly)))
           (polyval-calc
            (poly-monomials p)
            (create-pairs-kv (variables p) variable-values))))
        ))


; ritorna NIL dopo aver stampato sullo standard output una rappresentazione 
; tradizionale del termine polinomio associato a poly

(defun pprint-polynomial (poly)
  (cond ((is-monomial poly)
         (pprint-monomials (list poly)))
        
        ((is-polynomial poly)
         (pprint-monomials (poly-monomials poly))
         )
        
        (T
         (error "input non valido per la stampa del polinomio"))
        )
  )


; ritorna NIL dopo aver stampato sullo standard output una rappresentazione
; tradizionale del termine polinomio associato alla lista di monomi monomials
(defun pprint-monomials (monomials)
  (let ((m (first monomials))
        (c (m-coefficient (first monomials)))
        (vps (monomial-vars-and-powers (first monomials))))
    (cond ((eq monomials nil)
           (format t "~%"))
          
          (T
           (if (> c 0)
               (format t "+"))
           (format t "~S" c)
           (pprint-vars-and-powers vps)
           (pprint-monomials (cdr monomials)))
          ))
  )


; ritorna NIL dopo aver stampato sullo standard output una rappresentazione
; tradizionale dei termini varpower contenuti in vps
(defun pprint-vars-and-powers (vps)
  (let ((s (varpower-symbol (first vps)))
        (p (varpower-power (first vps))))
    (cond ((eq vps nil)
           (format t " "))
          
          (T
           (format t "*~S" s)
           (if (> p 1)
               (format t "^~D" p))
           (pprint-vars-and-powers (cdr vps)))
          ))
  )


;; end-of-file -- polinomi.LISP
