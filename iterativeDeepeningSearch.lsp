;;; 
 ;; File: iterativeDeepeningSearch
 ;; Author: Lourens Schep, 100103961
 ;; Date: November 27,2012
 ;; For: Project, COMP 3613, Fall, 2012
;;;


;;; (initiateMap)
 ;; initiates the hash table for the 
 ;; tree used for the iterative searching
 ;; uses nodes as a global variable.
;;;
(defun initiateMap ()
    (defparameter *nodes* (make-hash-table))
)

;;; (adEdge edge dis con)
 ;; -- edge is the node its connected to
 ;; -- dis is the distance between edge and con
 ;; -- con is the other node
 ;; It makes a edge in the hash table just like
 ;; a adjacency list.
;;;
(defun addEdge (edge dis con)
    (if (gethash edge *nodes*)
    (setf (gethash edge *nodes*) (cons (list con dis) (gethash edge *nodes*)))
    (setf (gethash edge *nodes*) (list (list con dis))))
)

;;; (remEdge edge)
 ;; -- edge is the edge to be removed.
;;;
(defun remEdge (edge)
    (remhash edge *nodes*)
)

;;; (printTable)
 ;; prints the hash table with its nodes.
;;;
(defun printTable ()
  (loop for key being the hash-keys of *nodes*
                using (hash-value value)
                        do (format t "Value: ~S Connects to: ~S~%" key value))
)

;;; (giveAllKeys)
;;; gives all the keys in the hashmap.
(defun giveAllKeys ()
    (setf keys (list))
    (loop for key being the hash-keys of *nodes*
          using (hash-value value)
          do (append keys (list key)))
    (return keys)
)

;;; (getCost a b)
 ;; Returns the cost from a to b
 ;; assuming the are attached.
;;; 
(defun getCost (a b)
    (setf connected (gethash a *nodes*))
    (second (first (remove-if-not #'(lambda (x)    (equal (first x) b)) connected)))
)

;;; (getNodesConnected a)
 ;; returns the nodes connected to node a
;;; 
(defun getNodesConnected(a)
    (setf nodes (gethash a *nodes*))
    (mapcar #'(lambda (x) (first x)) nodes)
)



;;; --------Iterative deepening depth first search ------

;;; (iterDeepSearch start goal)
 ;; -- start is the starting node
 ;; -- goal is the goal it tries to find
 ;; uses iterative deepening search to find
 ;; the goal starting from the given start node
 ;; it uses the function look with is a recursive
 ;; function.
;;; 
(defun iterDeepSearch(start goal)
    (setf depth 0)
    (loop while (>= depth 0) do
        (if (equal (look start goal depth) T) (return "Found"))
        (setf depth (+ 1 depth))
    )
)

;;; (look start goal depth)
 ;; -- start is the start node
 ;; -- goal is the node it is trying to find
 ;; -- depth is how far down you can go.
 ;; iterDeepSearch uses this recursive
 ;; function to find the goal if it does not
 ;; find it will go forever otherwise it 
 ;; will return found.
;;;
(defun look (start goal depth)
  (cond ((and (= depth 0) (equal start goal))
            T
         )
        ((> depth 0)
         (some #'(lambda (x) (look x goal (- depth 1))) (getNodesConnected start))
         )
        (t
          nil
          ))
)


;;; test function
(defun test ()
    (initiateMap)
    (addEdge 'a '1 'b)
    (addEdge 'a '1 'c)
    (addEdge 'b '1 'd)
    (addEdge 'b '1 'e)
    (addEdge 'c '1 'f)
    (addEdge 'c '1 'g)
    (addEdge 'd '2 'h)
    (addEdge 'd '2 'i)
    (addEdge 'e '1 'j)
    (addEdge 'e '3 'k)
    (format t "Table")
    (printTable)
    (format t "Start from a using goal as f~%")
    (print (iterDeepSearch 'a 'f))
    (format t "~%Start from a using goal as k~%")
    (print (iterDeepSearch 'a 'k))
    (format t "~% Cannot test one where it does not
            find a node since it will go forever which its suppose to do~%")
    
)


