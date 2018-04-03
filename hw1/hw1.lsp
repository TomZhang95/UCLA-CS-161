;;;NAME: Tianyang Zhang
;;;SID: 404743024

;;;Overall Comment:
;;;The solutions I wrote have very similar structure, they all
;;;start with one or more initial statement as the checking of
;;;special case and stop sign of the recursive call.
;;;If the initial statement all passed, then start the 
;;;recursive call to iterate through the whole list/tree


;;;Problem 1, TREE-CONTAINS;;;
;;;Takes two arguments N and TREE, and checks whether
;;;number N appears in the ordered tree TREE.
;;;
;;;
;;;Arguments: 
;;;
;;;N: Integer
;;;TREE: Ordered Tree
;;;
;;;
;;;Returns:
;;;Bool
;;;
(defun TREE-CONTAINS (N TREE)
	;;;If TREE is a number and N = TREE, return true.
	;;;If TREE is a number and N != TREE, return false.
	(if (numberp TREE) (= N TREE)
	(cond 
		;;;If TREE is a list, then compare N with the middle
		;;;element in TREE to determine which side of recursion 
		;;;should we go next.
		((= (second TREE) N) t)
		((> (second TREE) N) (TREE-CONTAINS N (first TREE)))
		(t (TREE-CONTAINS N (third TREE))))))



;;;Problem 2, TREE-MAX;;;
;;;Takes one argument TREE, and returns the
;;;maximum number appearing in the ordered tree TREE.
;;;
;;;
;;;Arguments: 
;;;
;;;TREE: Ordered Tree
;;;
;;;
;;;Returns:
;;;integer
;;;
(defun TREE-MAX (TREE)
	(if (numberp TREE) TREE
	(TREE-MAX (third TREE))))
;;;Recursive until the most right number which is the
;;;biggest.



;;;Problem 3, TREE-ORDER;;;
;;;Takes one argument TREE, and returns an
;;;in-ordered list of the numbers appearing in the 
;;;ordered tree TREE
;;;
;;;
;;;Arguments: 
;;;
;;;TREE: Ordered Tree
;;;
;;;
;;;Returns:
;;;list
;;;
(defun TREE-ORDER (TREE)
	(if (numberp TREE) 

		;;;If single number found, return it as a list,
		(list TREE)

		;;;Otherwise, append the first, second and third 
		;;;element in TREE together
		(append 
			(append (TREE-ORDER (first TREE)) 
				(list (second TREE)))
				(TREE-ORDER (third TREE)))))
;;;Append the list from most left number in TREE recursively

(print (TREE-ORDER 4))

;;;Problem 4, SUB-LIST;;;
;;;Takes a list L and two non-negative integers
;;;START and LEN, and returns the sub-list of L starting
;;;at position START and having length LEN.
;;;
;;;
;;;Arguments: 
;;;
;;;L: list
;;;START: integer
;;;LEN: integer
;;;
;;;
;;;Returns:
;;;list
;;;
(defun SUB-LIST (L START LEN)
	(cond 
		;;;Initial condition
		((= LEN 0) NIL)

		;;;Remove the first element until START=0
		((not (= START 0)) (SUB-LIST (cdr L) (- START 1) LEN))

		;;;Then make a list start with the first element in L
		;;;with length LEN
		((= START 0) (cons (car L) (SUB-LIST (cdr L) START 
			(- LEN 1))))
		(t NIL)))



;;;Problem 5, SPLIT-LIST;;;
;;;Takes a list L, and returns a list of two lists L1 and
;;;L2, in that order
;;;
;;;
;;;Arguments: 
;;;
;;;L: list
;;;
;;;
;;;Returns:
;;;list of 2 lists
;;;
(defun SPLIT-LIST (L)
	;;;Get the half of the length of L
	(let* ((len (/ (length L) 2)))
		(if (evenp (length L))
			;;;If L has even elements, then split with same lenth
			(append (list (SUB-LIST L 0 len)) (list (SUB-LIST L len len)))
			
			;;;If L has odd elements, then make second list contains
			;;;one more element
			(append (list (SUB-LIST L 0 (- len 1/2)) 
				(SUB-LIST L (- len 1/2) (+ len 1/2)))))))



;;;Problem 6, BTREE-HEIGHT;;;
;;;Takes a binary tree TREE, and returns the
;;;height of TREE.
;;;
;;;
;;;Arguments: 
;;;
;;;TREE: Binary Tree
;;;
;;;
;;;Returns:
;;;integer
;;;
(defun BTREE-HEIGHT (TREE)
	(if (numberp TREE)
		;;;If it's a atom, return height 0
		0

		;;;If it's a Binary Tree, both left and right height + 1
		(let* ((left (+ (BTREE-HEIGHT (first TREE)) 1)) 
			(right (+ (BTREE-HEIGHT (second TREE)) 1)))

		;;;Return the maximum height between left and right branch
		(if (> left right)
			left
			right))))



;;;Problem 7, LIST2BTREE;;;
;;;Takes a non-empty list of atoms LEAVES, and
;;;returns a binary tree
;;;
;;;
;;;Arguments: 
;;;
;;;LEAVES: list
;;;
;;;
;;;Returns:
;;;Binary Tree
;;;
(defun LIST2BTREE (LEAVES) 
	;;;If there is only one element in LEAVES, return it as number
	(if (= (length LEAVES) 1)
		(car LEAVES)

		;;;If there are 2 elements in LEAVES, return itself
		(if (= (length LEAVES) 2) 
			LEAVES

			;;;Otherwise, split it using SPLIT_LIST and call recursion
			(let* ((left (first (SPLIT-LIST LEAVES)))
				(right (second (SPLIT-LIST LEAVES))))
				(append (list (LIST2BTREE left))
					(list (LIST2BTREE right)))))))



;;;Problem 8, BTREE2LIST;;;
;;;Takes a binary tree TREE as input, and returns a
;;;list of atoms
;;;
;;;
;;;Arguments: 
;;;
;;;TREE: Binary Tree
;;;
;;;
;;;Returns:
;;;list
;;;
(defun BTREE2LIST (TREE)
	(if (numberp TREE)
		;;;If TREE is a number, then return it in list type
		(list TREE)

		;;;Otherwise append the first and second part of TREE
		(append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))))



;;;Problem 9, IS-SAME;;;
;;;Takes two LISP expressions E1 and E2
;;;whose atoms are all numbers, and checks whether the 
;;;expressions are identical.
;;;
;;;
;;;Arguments: 
;;;
;;;E1: list or atom
;;;E2: list or atom
;;;
;;;
;;;Returns:
;;;Bool
;;;
(defun IS-SAME (E1 E2)
	(cond
		;;;Initial statements:
		;;;If E1 E2 are NULL or pass all previous check,
		;;;return true
		((and (NULL E1) (NULL E2)) t)

		;;;If E1 and E2 have different length, return NIL
		((or (NULL E1) (NULL E2)) NIL)
		((and (numberp E1) (numberp E2)) (= E1 E2))

		;;;If E1 and E2 are not same type, return NIL
		((and (numberp E1) (not (numberp E2))) NIL)
		((and (numberp E2) (not (numberp E1))) NIL)

		;;;Recursively check all elements in the list
		(t (if (IS-SAME (car E1) (car E2))
			(IS-SAME (cdr E1) (cdr E2))
			NIL))))


