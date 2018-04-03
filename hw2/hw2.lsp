;;;Problem 1, DFS;;;
;;;Performs a depth-first search of a tree
;;;
;;;Arguments: 
;;;tree: list
;;;
;;;
;;;Returns:
;;;list
;;;
(defun DFS (tree)
	(if (atom tree)
		;;;If tree is a single number, return it as a list
		(if tree
			(list tree)
			;;;If tree is null, return a empty list
			'())
		;;;If tree is a list, then recursively append the first part
		;;;and the rest parts of the tree as a list
		(append (DFS (car tree)) (DFS (cdr tree)))
		)
	)



;;;Problem 2, Depth-First Iterative-Deepening;;;
;;;A set of functions that top-level function called DFID
;;;take two arguments, the list representation of the tree, and 
;;;an integer representing the maximum depth of the tree, and 
;;;returns  a single top-level list of the terminal nodes in the 
;;;order that they would be visited by a left-to-right depth-first 
;;;iterative-deepening search.
;;;
;;;Arguments: 
;;;tree: list
;;;depth: integer
;;;
;;;
;;;Returns:
;;;list
;;;

;;;Function to print the first level of the tree
(defun print_one_level (tree)
	(if (atom tree) 
		(if (null tree)
			'()
			(list tree))

		;;;Check the first element in the current tree list, if 
		;;;it's a atom, then append it to the list, otherwise, 
		;;;recursively check through all the tree list
		(if (atom (car tree))
			(append (list (car tree)) (print_one_level (cdr tree)))
			(print_one_level (cdr tree))
			)
		)
	)

;;;Function to print the selected level(depth)
(defun print_levels (tree depth)
	(cond
		;;;Initial conditions to stop recursion: 
		;;;If tree is null, return nil
		((null tree) NIL)

		;;;If tree is a element, return it as list type
		((atom tree) (list tree))

		;;;If depth is 0, return an empty list
		((= depth 0) '())

		;;;If depth is 1, print the current first level
		((= depth 1) (print_one_level tree))

		;;;If depth > 1, recursively append from left to right
		;;;in the tree
		(t (append
			(print_levels (car tree) (- depth 1))
			(print_levels (cdr tree) depth)))
		)
	)

;;;Top-level function
(defun DFID (tree depth)
	(cond
		;;;Initial statments to stop recursion
		;;;If tree is null, return NIL
		((null tree) NIL)

		;;;If depth < 0, recursion finished, return NIL
		((< depth 0) NIL)

		;;;Otherwise, recursively append the output of each level
		(t (append (DFID tree (- depth 1)) 
			(print_levels tree depth))) 
		)
	)



;;;Problem 3, id-dfs;;;
;;;A depth-first iterative-deepening solver for the 
;;;missionary-cannibal problem
;;;
;;;Arguments: 
;;;s: list
;;;depth: integer
;;;
;;;
;;;Returns:
;;;list
;;;

;;;FINAL-STATE takes a single argument (S), the current state, 
;;;and returns T if it is the goal state (3 3 NIL) and NIL 
;;;otherwise.
(defun final-state (s)
	(if (equal s '(3 3 NIL))
		T
		NIL)
	)


;;;NEXT-STATE returns the state that results from applying an operator to the
;;;current state. It takes three arguments: the current state (S), a number of
;;;missionaries to move (M), and a number of cannibals to move (C). It returns a
;;;list containing the state that results from moving that number of missionaries
;;;and cannibals from the current side of the river to the other side of the
;;;river. If applying this operator results in an invalid state (because there
;;;are more cannibals than missionaries on either side of the river, or because
;;;it would move more missionaries or cannibals than are on this side of the
;;;river) it returns NIL.
(defun next-state (s m c)
	(let* ((MISSIONARIES (first s))
		(CANNIBALS (second s))
		(m_otherside (- 3 (first s)))
		(c_othersize ( - 3 (second s)))
		)
	(cond
		;;;If try to move more than 2, return NIL
		((> (+ m c) 2) NIL)

		;;;If try to move more MISSIONARIES than we have, return NIL
		((> m MISSIONARIES) NIL)

		;;;If try to move more CANNIBALS than we have, return NIL
		((> c CANNIBALS) NIL)

		;;;If MISSIONARIES will be less than CANNIBALS after move,
		;;;return NIL
		((and (> (- MISSIONARIES m) 0) (< (- MISSIONARIES m) (- CANNIBALS c))) NIL)

		;;;If MISSIONARIES on the otherside will be less than
		;;;CANNIBALS after move, return NIL
		((and (> m_otherside 0) (< (+ m_otherside m) (+ c_othersize c))) NIL)

		;;;Otherwise, it means the arguments are valid, return the
		;;;new state list of the opposite side of the river
		(t (list (list (+ m_otherside m) (+ c_othersize c) (not (third s)))))
		)
	)
	)


;;;SUCC-FN returns all of the possible legal successor states to the current
;;;state. It takes a single argument (S), which encodes the current state, and
;;;returns a list of states that can be reached by applying legal operators to
;;;the current state.
(defun succ-fn (s)
	(append (next-state s 2 0) (next-state s 1 0) 
		(next-state s 1 1) (next-state s 0 1) (next-state s 0 2))
	)


;;;MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
;;;path from the initial state to the current state (PATH), the legal successor
;;;states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
;;;first-in first-out list of states; that is, the first element is the initial
;;;state for the current search and the last element is the most recent state
;;;explored. MULT-DFS does a single depth-first iteration to the given depth on
;;;each element of STATES in turn. If any of those searches reaches the final
;;;state, MULT-DFS returns the complete path from the initial state to the goal
;;;state. Otherwise, it returns NIL.
(defun mult-dfs (states path depth)
	(cond
		((< depth 0) NIL)
		((= (length states) 0) NIL)
		((final-state (car states)) (append path (list (car states))))
		(T (let* ((ret (mult-dfs (succ-fn (car states))
			(append path (list (car states))) (- depth 1))))
			(if (null ret)
				(mult-dfs (cdr states) path depth)
				ret
				)
			)
		)
		)
	)


;;;SINGLE-DFS does a single depth-first iteration to the given depth. It takes
;;;three arguments: a state (S), the path from the initial state to S (PATH), and
;;;the depth (DEPTH). If S is the initial state in our search, PATH should be
;;;NIL. It performs a depth-first search starting at the given state. It returns
;;;the path from the initial state to the goal state, if any, or NIL otherwise.
(defun single-dfs (s path depth)
	(mult-dfs (succ-fn s) (append path (list s)) (- depth 1))
	)


;;;ID-DFS is the top-level function. It takes two arguments: an initial state (S)
;;;and a search depth (DEPTH). ID-DFS performs a series of depth-first
;;;iterations, starting from the given depth until a solution is found. It
;;;returns the path from the initial state to the goal state. The very first call
;;;to ID-DFS should use depth = 0.
(defun id-dfs (s depth)
	(let* ((ret (single-dfs s NIL depth)))
		(cond
			((null ret) (id-dfs s (+ depth 1)))
			(t ret)
			)
		)
	)

