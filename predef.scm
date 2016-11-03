(define (not x)
	(if x #f #t))

(define (odd? x)
	(= (modulo x 2) 1))

(define (even? x)
	(not (odd? x)))

(define (append l x)
	(if (null? l)
		x
		(cons (car l) (append (cdr l) x))))

(define (list-tail l n)
	(if (or (= n 0) (null? l))
		l
		(list-tail (cdr l) (+ n -1))))

(define (memq x l)
	(cond
		((null? l) #f)
		((eq? x (car l)) l)
		(else (memq x (cdr l)))))

(define (memv x l)
	(cond
		((null? l) #f)
		((eqv? x (car l)) l)
		(else (memv x (cdr l)))))

(define (member x l)
	(cond
		((null? l) #f)
		((equal? x (car l)) l)
		(else (memq x (cdr l)))))

(define (eqv? x y)
	(or (eq? x y)
			(and (number? x)
					 (number? y)
					 (= x y))))

(define-macro (case key . clauses)
	(define (clauser clauses)
		(if (null? clauses) '()
			(let ((this (car clauses)))
					(cons
						(list (list 'memv 'e (list 'quote (car this)))
									(cadr this))
						(clauser (cdr clauses))))))
		(list (list 'lambda '(e)
					 (cons 'cond (clauser clauses)))
		key))

