; sat-solver on CNF delta over n variables.
(defun sat? (n delta)
    (DFS n delta NIL)
)

; Run DFS, adding to the assignment list assign the assignments
; that would make our CNF delta evaluate to true.
; n is the number of variables.
(defun DFS (n delta assign)
    (cond ((= (length assign) n) 
        (cond 
            ((goal-test n delta assign) assign)
            (t NIL)))
        ((inconsistency? delta assign) NIL)
        ; This is forward checking

        ; Select next variable based on next-var function.
        ; We try to select least constraint variable
        ((< (length assign) n)
            (let* 
                ((nextVar (next-var delta assign n))
                (cleanedDelta (rm-sat-clauses delta assign))
                ; We clean up the CNF by getting rid of clauses that 
                ; are already evaluated to TRUE.

                (left (DFS n cleanedDelta (assign-var nextVar NIL assign))))
            (cond 
                ((not (null left)) left)
                (t (DFS n cleanedDelta (assign-var nextVar T assign))))))
        (t NIL)))

; Returns the next variable to be considered for assignment.
; The next variable is var + 1 if there is no unit clause.
; Otherwise, pick the variable in the unit clause.
(defun next-var (delta assign n)
    (let* ((unitClause (unit-clause delta)))
        (cond ((null unitClause) (lowest-unassigned-var assign n))
            ((var-assigned? (abs (car unitClause)) assign) (lowest-unassigned-var assign n))
            (t (abs (car unitClause))))))

; Returns the lowest unassigned variable, given
; The current assignment list assign and the max variable n.
(defun lowest-unassigned-var (assign n)
    (lowest-unassigned-var-helper assign 1 n))

; Call with lowest = 1
(defun lowest-unassigned-var-helper (assign lowest n)
    (cond ((> lowest n) lowest)  ; we should never get here
        ((not (var-assigned? lowest assign)) lowest)
        (t (lowest-unassigned-var-helper assign (+ lowest 1) n))))

; Returns the CNF minus the clauses that are satisfied by the assignment
; assign, given current CNF delta.
(defun rm-sat-clauses (delta assign)
    (cond ((null delta) NIL)
        ((null assign) delta)

        ; if the first clause evaluates to true, we omit it.
        ((eval-clause (first delta) assign)
            (rm-sat-clauses (rest delta) assign))
        (t (cons (first delta) (rm-sat-clauses (rest delta) assign)))))

; Return delta - {satisfied clauses}
;(defun rm-sat-clauses (delta assign)
;    (cond ((null delta) T)
;        ((null assign) delta)
;        ((eval-clause (first delta) assign) 
;            (rm-sat-clauses (rest delta) assign))   
;        (t (cons (first delta) (rm-sat-clauses (rest delta) assign)))
;    )
;)

;; Returns the next variable to be assigned a value.
;; Next variable is the in the first unit clause from the left.
;; If there is no unit clause, we choose the first atom in delta.
;(defun next-var (delta)
;    (cond ((null delta) NIL)
;        (t 
;        (let* ((unitClause (unit-clause delta)))
;            (cond ((not (null unitClause)) (abs (car unitClause)))
;                (t (abs (first (first delta)))))))))

; Returns unit clause in CNF delta. If none exist, return NIL.
(defun unit-clause (delta)
    (cond ((null delta) NIL)
        ((= (length (first delta)) 1) (first delta))
        (t (unit-clause (rest delta)))))

;; Return the next unassigned variable to be assigned a value,
;; looking at the CNF delta. Variable is unassigned if it is 
;; not in the current assignment list assign.
;(defun next-var (delta assign)
;    (cond ((null delta) NIL)
;        (t
;        (let* ((var (next-var-by-clause (smallest-clause delta) assign)))
;            (cond ((null var) (next-var (rest delta) assign))
;                (t var))))))
;
;; Return the next unassigned variable to be assigned a value, looking at
;; a clause. The current assignment list assign is used to check that
;; the variable has not yet been assigned.
;(defun next-var-by-clause (clause assign)
;    (cond ((null clause) NIL)
;        ; if first var in clause not yet assigned
;        ((not (var-assigned? (abs (first clause)) assign))
;            (abs (first clause)))
;       (t (next-var-by-clause (rest clause) assign))))

; Is the variable var assigned, i.e. in the assignment list assign?
(defun var-assigned? (var assign)
    (cond ((null assign) NIL) 
        ((= var (abs (first assign))) T)
        (t (var-assigned? var (rest assign)))))

(defun goal-test (n delta assign)
    (and (= n (length assign)) (not (inconsistency? delta assign))))

; remove clause from CNF delta
;(defun remove-clause (clause delta)
;    (cond ((null delta) NIL)
;        ((equal clause (first delta)) (rest delta))
;        (t (cons (first delta) (remove-clause clause (rest delta))))))

; Returns whether there is an inconsistency in the
; current assignment assign, given the CNF delta.
; CNF is inconsistent if one of the clauses is inconsistent.
; Empty delta is considered inconsistent.
(defun inconsistency? (delta assign)
    (cond ((null delta) T)
        ((null assign) NIL)
        (t (inconsistency?-helper delta assign))))

(defun inconsistency?-helper (delta assign)
    (cond ((null delta) NIL) 
        ; if we don't do this, we'd always return NIL

        (t (or (clause-inconsistency? (first delta) assign)
            (inconsistency?-helper (rest delta) assign)))))

; Returns whether a clause is inconsistent based on current
; set of assignments assign. Clause is inconsistent if
; all literals in the clause evaluate to false.
; Empty clause is considered inconsistent.
(defun clause-inconsistency? (clause assign)
    (cond ((null assign) NIL)
        ((null clause) T)
        ((not (full-clause-assigned? clause assign)) NIL)
        ; if not every variable is assigned, there may be 
        ; a variable that would set clause TRUE. Therefore,
        ; We don't have an inconsistency.

        (t (clause-inconsistency?-helper clause assign))))

(defun clause-inconsistency?-helper (clause assign)
    (cond ((null clause) T) 
        ((null assign) T)
        (t (and (not (eval-literal (first clause) assign))
            (clause-inconsistency?-helper (rest clause) assign)))))

; Determines whether a clause has all its variables assigned.
; assign is the set of assignments.
(defun full-clause-assigned? (clause assign)
    (cond ((null assign) NIL) 
        ((null clause) T)  ; empty clause has full assignment

        ; If the first literal is assigned, check the rest of the clause
        ((var-assigned? (abs (first clause)) assign) 
            (full-clause-assigned? (rest clause) assign))
        (t NIL)))

; Call only if we have a full clause assignment.
; Returns evaluation of clause, given assignment assign.
(defun eval-clause (clause assign)
    (cond ((null clause) NIL)
        ((null assign) NIL)
        (t (or (eval-literal (first clause) assign) 
            (eval-clause (rest clause) assign))))
)

; Returns whether variable var has an assignment.
; assign is a list of assignments.
(defun var-assigned? (var assign)
    (cond ((null assign) NIL)
        ((= var (abs (first assign))) T)
        (t (var-assigned? var (rest assign)))))

; return T/F value of literal, given assignment list assign
; literal is an atom, and assign is a list
(defun eval-literal (literal assign)
    (cond 
        ((null assign) NIL)
        ((= (abs literal) (abs (first assign)))
            (cond ((< literal 0) (not (get-val (first assign))))
                (t (get-val (first assign)))))
    (t (eval-literal literal (rest assign))))
)

; returns actual value of variable
(defun get-val (x)
    (cond ((< 0 x) T) (t NIL)))

; adds the new assignment of variable x to the value (T/F) val
; and returns the list assign with the new variable appended.
(defun assign-var (x val assign)
    (append assign (list (assign-var-helper x val))))

; returns the representation of x = T/F.
; If x = T, return x. Otherwise, return -x.
(defun assign-var-helper (x val)
    (cond ((null val) (- x)) (t x)))

;(defun smallest-clause (delta) 
;    (let* 
;        ((minClause (first delta))
;        (minSize (length minClause)))
;        (smallest-clause-helper minSize minClause delta)
;    )    
;)
;
;; call with minSize = (length (first delta))
;(defun smallest-clause-helper (minSize minClause delta)
;    (cond
;        ((null delta) minClause)
;        ((= (length delta) 1) 
;            (cond ((> minSize (length (first delta))) (first delta))
;                (t minClause)
;            ))
;        ((> minSize (length (first delta)))
;            (smallest-clause-helper (length (first delta)) (first delta) (rest delta)))
;        (t (smallest-clause-helper minSize minClause (rest delta)))
;    )
;)