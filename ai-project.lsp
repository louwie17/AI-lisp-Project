
(defun initiateMap ()
    (defparameter *nodes* (make-hash-table))
)

(defun addEdge (edge dis con)
    (if (gethash edge *nodes*)
    (setf (gethash edge *nodes*) (cons (list con dis) (gethash edge *nodes*)))
    (setf (gethash edge *nodes*) (list (list con dis))))
)

(defun remEdge (edge)
    (remhash edge *nodes*)
)

(defun printTable ()
  (loop for key being the hash-keys of *nodes*
                using (hash-value value)
                        do (format t "The value associated with the key ~S is ~S~%" key value))
)
;; assuming they are attached
(defun getCost (a b)
    (setf connected (gethash a *nodes*))
    (second (first (remove-if-not #'(lambda (x)    (equal (first x) b)) connected)))
)

(defun getNodesConnected(a)
    (setf nodes (gethash a *nodes*))
    (mapcar #'(lambda (x) (first x)) nodes)
)

(defun test ()
    (initiateMap)
    (addEdge 'a '1 'b)
    (addEdge 'a '1 'c)
    (addEdge 'b '1 'd)
    (addEdge 'b '1 'e)
    (addEdge 'c '1 'f)
    (addEdge 'c '1 'g)
    (print (getCost 'a 'b))
    (printTable)
    (iterDeepSearch 'a 'f)
)


;;; --------Iterative deepening depth first search ------
;;; a is starting node 
;;; b is the goal
(defun iterDeepSearch(start goal)
    (setf depth 0)
    (loop while (>= depth 0) do
        (if (equal (look start goal depth) T) (return "Found"))
        (setf depth (+ 1 depth))
    )
)

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


;; ---------------- a star search --------------
(defstruct node

(defun astar (start goal)
    (setf closedset ())
    (setf openset ()) ;; get all the nodes that are in the hashmap

    (loop while (> (length openset) 0)

    
    )
  
)
