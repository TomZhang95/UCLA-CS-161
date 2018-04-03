
;Given a clause as list, return true if it contains 'var',
;otherwise return NIL
(defun is-true-clause (clause var)
	(cond
		((null clause) NIL)
		((= var (car clause)) T)
		(t (is-true-clause (cdr clause) var))
		)
	)

;Removing negate of 'var' inside current clause
(defun remove-var (clause var)
	(cond
		((null clause) NIL)
		((= (- 0 var) (car clause)) (remove-var (cdr clause) var))
		(t (append (list (car clause)) (remove-var (cdr clause) var)))
		)
	)


;Given a CNF list, remove the clauses which contains 'var', and
;remove all the negate of 'var' in each clause.
;Return a new CNF list;
(defun remove-true-clause (delta var)
	(cond
		((null delta) NIL)
		((is-true-clause (car delta) var) (remove-true-clause (cdr delta) var))
		(t (append (list (remove-var (car delta) var)) (remove-true-clause (cdr delta) var)))
		)
	)


;Helper function to find the shortest clause, which means finding
;the least legal value variable. Given a CNF list, return the clause 
;which has smallest length, if multiple clause has same length, 
;return the one found first
(defun shortest-clause (delta)
	(if (null delta) 
		NIL
	(let*
		((clause_a (car delta)))
		(let*
			((clause_b (shortest-clause (cdr delta))))
			(if (null clause_b)
				clause_a
				(if (< (length clause_a) (length clause_b))
					clause_a
					clause_b
					)
			)
		)
	))
	)


;Helper function to check if there is empty clause in current delta,
;if so, it means the current result of backsearch is not working, which
;need to try to set values by using the rest of shortest clause elements.
(defun contain-null (delta)
	(cond
		((null delta) NIL)
		((null (car delta)) T)
		(t (contain-null (cdr delta)))
		)
	)

;Given a CNF list and un-assigned variable, try to set the variable to
;true, if it works, return the resulted assigned variable list, otherwise,
;try to set the variable to false.
(defun assigned (delta s-clause)
	(if (and (null s-clause) (null delta))
		NIL
		(if (null s-clause)
			(list '())
			(let* ((newdelta (remove-true-clause delta (car s-clause))))
				(if (contain-null newdelta)
					(assigned delta (cdr s-clause))
					(append (list (car s-clause)) (assigned newdelta (shortest-clause newdelta)))
					)
				)
			)
		)
	)

;Helper function to check if there is NIL inside the result list
(defun contain-not-number (result)
	(cond
		((null result) NIL)
		((not (numberp (car result))) T)
		(t (contain-not-number (cdr result)))
		)
	)


;Helper function for supplement. Checking if there is 'val' inside
;list l
(defun contain-val (l val)
	(cond
		((null l) NIL)
		((or (= (car l) val) (= (car l) (- 0 val))) T)
		(t (contain-val (cdr l) val))
		)
	)


;Supplementing the result, if there is no n inside result list, append it to
;current result, and iterate through all numbers from 1 to n
(defun supplement (n result)
	(cond
		((= n 0) result)
		((not(contain-val result n)) (cons n (supplement (- n 1) result)))
		(t (supplement (- n 1) result)) 
		)
	)


;Top level function, call the function 'assigned' to get a result list, if
;there is one or more NIL inside result list, it means 'delta' is unsat.
;If elements in results are all numbers, return the result.
(defun sat? (n delta)
	(if (null delta)
		NIL
		(let* ((result (assigned delta (shortest-clause delta))))
			(if (contain-not-number result)
				NIL
				(supplement n result)
				)
			)
		)
	)

