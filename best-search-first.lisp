;;; Author     : Drew Heasman
;;; Date       : 13 December 2020
;;;
;;; To run the program call the shortest path algorithm call spa with start node, goal node,
;;; the graph function, and heuristic functions.
;;; e.g. (spa 'a 'd #'graph #'heuristic) or (spa 'f 'j '#graph '#heuristic)
;;;
;;; Expected results: (spa 'a 'd #'graph #'heuristic)                               --> (A B C D) . 6)
;;;                   (spa 'f 'j #'graph #'heuristic) while setting heuristics to 0 --> (F G I J) . 4)
;;;                   (spa 'f 'j #'graph #'heuristic) while using heuristics        --> (F J) . 5)
;;;
;;; Help, particularily with creating hash tables and formatting the algorithm properly was
;;; obtained from CSG120: Foundations of Artificial Intelligence,Prof. Ronald J. Williams,
;;; https://www.ccs.neu.edu/home/rjw/csg120/


;;; Defined graph as a function, pass in state and return edges.
;;; ((node) '(edges)). 2 seperate graphs are defined as examples.
(defun graph (state)
  (case state
	((a) '((b . 3)))                  ; Graph as expressed in the Assignment
	((b) '((c . 1) (d . 5)))
	((c) '((d . 2)))
        ((d) '((b . 2)))
         
	((f) '((g . 1) (j . 5)))          ; More complicated graph to show the use of A* heuristic
	((g) '((i . 2) (f . 4)))
	((h) '((i . 2) (g . 3)))
        ((i) '((j . 1)))
        ((j) '((f . 2) (i . 3))) 
	))

;;; A* Hueristic
(defun heuristic (state goal)
  (if (eql goal 'j)			; Apply the heuristic if the goal state is j
      (case state			; This heuristic is only set up for goal j
	    ((f) 1.0)                   ; The heuristic is arbitrary and could represent any new weight
	    ((g) 5.0)                   ; system for a pathfinding algorithm. (e.g. traffic, euclidean distance)
	    ((h) 8.0)
	    ((i) 5.0)
            ((j) 0.0)
	    )
       0.0				; Any other goal state will have 0.0 
       ))

;;; Node structure as a path, path-length, and total-length estimate
(defstruct node                         
  path
  path-length
  total-length-estimate
  )

;;; Less-than function to determine if x is less than y. y can be infinite (in this program represented as null)
(defun less-than (x y)
  (or (null y) (< x y)))

;;; Shortest path algorithm with heuristic addition.
(defun spa (start goal graph heuristic)
  (do (head-node	; Define the head-node(top of the open-list)
       path-to-extend	; Define the path currently visited, that needs to be explored
       current-state	; Define the current state
       dist-so-far	; Define the length of the path being explored.
       extended-paths	; Define the list of newly extended paths.
       (open		; This list is the list of available unexplored node structs 
	(list (make-node :path (list start)       ; Initialize the node-path as the starting node
			 :path-length 0           ; Initialize the node-path-length as 0
			 :total-length-estimate   ; Set the node-total-length-est based on the defined heuristics
			 (funcall heuristic start goal))))
       (state-distances (make-hash-table :test #'equalp))
       )
      ((null open) nil)	                          ; Fail condition. No path to goal is found.
      (setq head-node (pop open))                 ; Pop the top of the open list and assign to the head-node
      (setq path-to-extend (node-path head-node)) ; Set the path to extend as the head-node-path
      (setq current-state (car path-to-extend))   ; Set the current state as the first cell of path-to-extend
      (if (equalp current-state goal)
	  (return (cons (reverse (node-path head-node))
                        (node-path-length head-node)))) ; Success condition. Return the path and the cost of path
      (setq dist-so-far (node-path-length head-node))   ; Set the dist-so-far as the head-node's path-length
      (when (less-than dist-so-far (gethash current-state state-distances))  ; Begin the algoritm
	 (setf (gethash current-state state-distances) dist-so-far) ; Create a hash-table
	 (let (next-state
	       next-dist-so-far
	       (next-nodes nil))
	   (dolist (pair (funcall graph current-state))             ; Set the next-state and dist-so-far
	     (setq next-state (car pair))
	     (setq next-dist-so-far (+ (cdr pair) dist-so-far))
	     (if (less-than next-dist-so-far                        ; If we have found a new, shorter path, 
			    (gethash next-state state-distances))   ; make a new node and merge with the open-list
		 (setf open
		       (merge
			'list
			(list
			 (make-node
			  :path (cons next-state path-to-extend)
			  :path-length next-dist-so-far
			  :total-length-estimate
			  (+ next-dist-so-far
			     (funcall heuristic next-state goal))))
			open
			#'<
			:key #'node-total-length-estimate)))
		)))
      ))


