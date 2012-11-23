
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
    (look 'a 'd 1)
    (iterDeepSearch 'a 'g)
)

;;; a is starting node 
;;; b is the goal
(defun iterDeepSearch(start goal)
    (setf depth 1)
    (setf foundGoal null)
    (loop while (= foundGoal T) do
        (setf foundGoal (look start goal depth)) 
        (setf depth (+ depth 1))
    )
)

(defun look (start goal depth)
  (cond ((and (= depth 0) (equal start goal))
            T
         )
        ((> depth 0)
         (mapcar #'(lambda (x) (look x goal (- depth 1))) (getNodesConnected start))
         )
        (t
          nil
          ))
)
