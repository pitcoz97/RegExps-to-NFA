;;; -*- Mode: Lisp -*-
;;;
;;; nfa.lisp --
;;;
;;; Linguaggi di programmazione 2019-2020
;;; Progetto Lisp gennaio 2020 E1P
;;; Picozzi Riccardo - Mat: 816974
;;; Paganini Andrea - Mat: 816028

;;;;;;;;;;;;;;;;;;;; IS-REGEXP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; controlla se RE è una espressione regolare
(defun is-regexp (re)
  	(cond 
  		((symb re) T)
        ((and 
        	(> (length re) 2) 
            (eql (first re) 'seq)) 
         		(rest-list (rest re)))
        ((and 
        	(> (length re) 2) 
            (eql (first re) 'or)) 
         		(rest-list (rest re)))
        ((and 
        	(= (length re) 2) 
            (eql (first re) 'star)) 
         		(rest-list (rest re)))
        ((and 
        	(= (length re) 2) 
            (eql (first re) 'plus)) 
         		(rest-list (rest re)))))

; controlla se RE è un simbolo dell'alfabeto
(defun symb (re)
	(cond 
		((atom re) T)
		((and 
			(not(eql(first re) 'star)) 
            (not(eql(first re) 'seq)) 
            (not(eql(first re) 'or)) 
            (not(eql(first re) 'plus))) T)
		(T nil)))

(defun rest-list (re)
  	(cond 
  		((atom re) T)
        (T  
        	(and (is-regexp (first re)) 
                (rest-list (rest re))))))

;;;;;;;;;;;;;;;;;;;; NFA-REGEXP-COMP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; compilatore di espressioni regolari(RE)
(defun nfa-regexp-comp (re)
	(if (is-regexp re)
		(append 
			(list '(initial 0)) 
			(compile-re re 0 -1) 
			(list '(final -1)))
		nil))

(defun compile-re (re init fin)
	(cond 
		((symb re)
			(list (list 'delta init re fin)))
		((eql (first re) 'star) 
			(append 
				(star-compiler (rest re) init fin)
				(list (list 'delta init 'epsilon fin)))) 
		((eql (first re) 'or) 
			(or-compiler (rest re) init fin))
		((eql (first re) 'seq) 
			(seq-compiler (rest re) init fin))
		((eql (first re) 'plus)
			(plus-compiler (rest re) init fin))))

; compilatore per star(<RE>)
(defun star-compiler (re init fin) 
	(if (symb (first re))
		(list (list 'delta fin (first re) fin))
		(compile-re (first re) fin fin)))

; compilatore per plus(<RE>)
(defun plus-compiler (re init fin)
	(if (symb (first re))
		(append (list (list 'delta init (first re) fin))
			(list (list 'delta fin (first re) fin)))
		(append (compile-re (first re) init fin)
			(list (list 'delta fin 'epsilon init)))))

; compilatore per or(<RE1>, ..., <REn>)
(defun or-compiler (re init fin)
	(if (not (null re))
		(if (symb (first re))
			(append (list (list 'delta init (first re) fin))
				(or-compiler (rest re) init fin))
			(append (compile-re (first re) init fin)
				(or-compiler (rest re) init fin)))
		nil))

; compilatore per seq(<RE1>, ..., <REn>)
(defun seq-compiler (re init fin)
	(if (not (null re))
		(cond 
			((and 
				(symb (first re)) 
				(not (null (rest re))))
					(append 
						(list (create-delta (first re) init))
						(seq-compiler 
							(rest re) 
							(fourth (create-delta (first re) init)) 
							fin)))
			((and 
				(symb (first re)) 
				(null (rest re)))
					(append 
						(list (list 'delta init (first re) fin))))
			((and 
				(not (symb (first re))) 
				(not (null (rest re))))
			  		(append 
			  			(compile-re 
			  				(first re) 
			  				init 
			  				(fourth (create-delta (first re) init)))
			  			(seq-compiler 
			  				(rest re) 
			  				(fourth (create-delta (first re) init)) 
			  				fin)))
			((and 
				(not (symb (first re))) 
				(null (rest re)))
			  		(append 
			  			(compile-re (first re) init fin))))
		nil))
	
(defun create-delta (re f)
	(list 'delta f re (+ f 1)))


;;;;;;;;;;;;;;;;;;;; NFA-TEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; restituisce true se l'input inserito viene consumato 
; completamente giungendo in uno stato d'accettazione (=-1)
(defun nfa-test (nfa input)
	(if (atom input)
		nil
		(if (= (cicle-input (delta-list nfa) input 0) -1)
			T
			nil)))

; restituisce una lista coi soli delta (per comodità)
(defun delta-list (nfa)
	(cond 
		((eql (first (first nfa)) 'delta)
			(append 
				(list (rest (first nfa))) 
				(delta-list (rest nfa))))
		((not (null nfa)) 
			(delta-list (rest nfa)))))

; restituisce lo stato raggiunto dall'input singolo dato
(defun accept-symb (nfa input stato)
	(cond 
		((and 
			(eql (second (first nfa)) 'epsilon) 
			(= (first (first nfa)) stato)) 
				(append (list (third (first nfa))) 'epsilon))
		((and 
			(eql (second (first nfa)) input) 
			(= (first (first nfa)) stato))
				(append (list (third (first nfa))) input))
		((not (null nfa))
			(accept-symb (rest nfa) input stato))
		(T (append (list -100) 'err))))

; cicla la lista in input
(defun cicle-input (nfa input stato)
	(if (not (null (rest input)))
		(if (eql (cdr (accept-symb nfa (first input) stato)) 'epsilon)
			(funct
				nfa 
				input 
				(first (accept-symb nfa (first input) stato)))
			(funct
				nfa 
				(rest input) 
				(first (accept-symb nfa (first input) stato))))
		(if (not (eql (cdr (accept-symb nfa (first input) stato)) 'epsilon))
			(first 
				(accept-symb nfa (first input) stato))
			(first 
				(accept-symb 
					nfa 
					(first input) 
					(first (accept-symb nfa (first input) stato)))))))

(defun funct (nfa input stato)
	(cicle-input nfa input stato))


;;; end of file -- nfa.lisp --
