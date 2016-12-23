;;;; -*- Mode: LISP -*-

;; polinomi.LISP


; Monomio : (m coefficent total-degree vars-n-powers)
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
(defun monomial-coefficent (m)
  (second m))


; estrae il grado totale da un monomio
(defun monomial-total-degree (m)
  (third m))


; estrae la lista di variabili da un monomio
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


; si occupa della semplifica del monomio
(defun semplify-monomial (m)
  (cond ((is-monomial m)
         (list 'm 
               (monomial-coefficent m) 
               (monomial-total-degree m)
               (semplify-vars-and-powers (monomial-vars-and-powers m))))

        (T
         (error "input non valido per la semplifica del monomio")))
  )


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


; si occupa della semplifica del polinomio
(defun semplify-polynomial (poly)
  poly
  )


; elimina monomi con coefficente 0
(defun zero-eater-m (ms)
  ms
  )


; 1   se vps1 = vps2
; 0   se vps1 < vps2
; nil se vps1 > vps2
(defun vars-and-powers<= (vps1 vps2)

  (if (and (every #'is-varpower vps1)
           (every #'is-varpower vps2))

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
               
    (error "Input non valido per il confronto tra liste di variabili")
    ))       


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
              
              ((and (= td1 td2)
                    (eq (vars-and-powers<= vps1 vps2) 0))
                    0)

              ((and (= td1 td2)
                    (eq (vars-and-powers<= vps1 vps2) nil))
               nil)

              ((and (= td1 td2)
                    (eq (vars-and-powers<= vps1 vps2) 1))
               1)
              
              (T
               (error "Durante il confronto tra monomi"))
              ))

    (error "Input non valido per il confronto tra monomi")
    ))


; riordina una lista di monomi secondo le specifiche
(defun sort-monomials (monomials)
  (sort monomials #'ms<=)
  )


;; end-of-file -- polinomi.LISP
