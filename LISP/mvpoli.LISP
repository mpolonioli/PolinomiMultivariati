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
(defun monomial-coefficient (m)
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
               (monomial-coefficient m) 
               (monomial-total-degree m)
               (semplify-vars-and-powers (monomial-vars-and-powers m))))
        
        ((eq m nil)
         m
         )
        
        (T
         (error "input non valido per la semplifica del monomio")))
  )

; data una lista di strutture Monomial torna la lista di strutture Monomial semplificate
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
           (error "input non valido per l'eliminazione delle variabili con esponente zero")))
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
               (error "simboli di variabile non validi durante il confronto tra variabili"))
              ))
    (error "input non valido per il confronto tra variabili"))
  )


; riordina le variabili secondo le specifiche
(defun sort-vars-and-powers (vps)
  (sort vps #'vp<=)
  )


; restituisce un polinomio parsato, semplificato e riordinato
(defun as-polynomial (input)
  (cond ((eq (first input) '+)
      (semplify-polynomial (parse-polynomial (cdr input) ())))
        (T
         (error "input non valido per il parsing del polinomio"))
    ))


; si occupa del parsing del polinomio
(defun parse-polynomial (ms app)
  (cond ((eq ms ())
         (list 'poly app))
        
        (T
         (parse-polynomial (cdr ms) (append (list (as-monomial (first ms))) app)))
        ))


; data una struttura Poly torna la struttura Poly semplificata
(defun semplify-polynomial (poly)

  (if (is-polynomial poly)

      (let ((ms (sort-monomials (zero-eater-m (semplify-monomials (poly-monomials poly) ()) ()))))
        (semplify-polynomial-r (first ms) (cdr ms) ())
        )
    
    (error "input non valido per la semplifica del polinomio")
    ))

; data una lista di strutture Monomial torna la lista di strutture Monomial semplificata
(defun semplify-polynomial-r (m1 ms app)

  (let ((m2 (first ms))
        (vps1 (monomial-vars-and-powers m1))
        (vps2 (monomial-vars-and-powers m2))
        (c1 (monomial-coefficient m1))
        (c2 (monomial-coefficient m2))
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
    
          ((= (monomial-coefficient m) 0)
           (zero-eater-m (cdr ms) app))

          ((not (= (monomial-coefficient m) 0))
           (zero-eater-m (cdr ms) (append app (list m))))
          
          (T
           (error "durante l'eliminazione dei monomi con coefficente nullo"))
          ))
  )


; vps1 e vps2 devono avere simboli di variabile identici e grado totale identico
; 1   se vps1 = vps2
; 0   se vps1 < vps2
; nil se vps1 > vps2
(defun playoff-vars-and-powers<= (vps1 vps2)

  (let ((vp1 (first vps1))
        (vp2 (first vps2)))
    
    (cond ((and (eq vp1 nil)
                (eq vp2 nil))
           1)
          
          ((and (eq vp1 nil)
                (not (eq vp2 nil)))
           0)
          
          ((and (not (eq vp1 nil))
                (eq vp2 nil))
           nil)
          
          ((< (varpower-power vp1) (varpower-power vp2))
           0)
          
          ((> (varpower-power vp1) (varpower-power vp2))
           nil)
          
          ((= (varpower-power vp1) (varpower-power vp2))
           (vars-and-powers<= (cdr vps1) (cdr vps2)))
          
          (T 
           (error "durante il confronto tra liste di variabili"))
          ))
  )


; vps1 e vps2 devono avere grado totale identico e simboli di variabili diversi
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
          
          ((eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) 0)
           0)
          
          ((eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) nil)
           nil)
          
          ((eq (string<= (varpower-symbol vp1) (varpower-symbol vp2)) 1)
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
            (vps2 (monomial-vars-and-powers m2))
            (variables1 (variables m1))
            (variables2 (variables m2)))
        
        (cond ((< td1 td2)
               0)

              ((> td1 td2)
               nil)
              
              ((and (= td1 td2)
                    (not (equal variables1 variables2)))
               (vars-and-powers<= vps1 vps2))

              ((and (= td1 td2)
                    (equal variables1 variables2))
               (playoff-vars-and-powers<= vps1 vps2))
              
              (T
               (error "Durante il confronto tra monomi"))
              ))

    (error "Input non valido per il confronto tra monomi")
    ))


; riordina una lista di monomi secondo le specifiche
(defun sort-monomials (monomials)
  (sort monomials #'ms<=)
  )


; data una lista di varpower torna la lista dei simboli di variabile in essa
; senza ripetizioni e ordinata in ordine lessicografico crescente
(defun vars-and-powers-variables (vps app)
  
  (let ((vp (first vps)))
    
    (cond ((is-varpower vp)
           (vars-and-powers-variables (cdr vps)
                                      (append app (list (varpower-symbol vp)))))
          
          ((eq vp nil)
           (sort (remove-duplicates app) #'string<=))
          
          (T
           (error "Input non valido per l'estrazione dei simboli di variabile da vps"))
          ))
  )


; data una lista di monomi torna la lista dei simboli di variabile contenuti in essa
; senza ripetizioni e ordinata in ordine lessicografico crescente
(defun monomials-variables (ms app)
  
  (let ((m (first ms)))
    
    (cond ((eq m nil)
          ((sort (remove-duplicates app) #'string<=)))
          
          ((is-monomial m)
           (monomials-variables (cdr ms) 
                                (append app (vars-and-powers-variables 
                                             (monomial-vars-and-powers m))))
           )

          (T
           (error "Input non valido per l'estrazione dei simboli di varabile da ms"))
          
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
    (error "l'input di varpowers deve essere una struttura Monomial")
    ))

; data una struttura Monomial, ritorna la lista di variabili Variables
(defun vars-of (Monomial)

  (if (is-monomial monomial)
      (monomial-variables monomial ())
    (error "l'input di vars-of deve essere una struttura Monomial"))
  )
  

;; end-of-file -- polinomi.LISP
