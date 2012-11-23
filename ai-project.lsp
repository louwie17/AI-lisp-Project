
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
    (addEdge 'a '12 'b)
    (addEdge 'a '6 'c)
    (addEdge 'b '15 'c)
    (print (getCost 'a 'b))
    (printTable)
    (getNodesConnected 'a)
)

;;; a is starting node 
;;; b is the goal
(defun iterDeepSearch(a b)
    (setf depth 1)
    (setf foundGoal null)
    (loop while (= foundGoal T) do
        (setf foundGoal (look a b depth)) 
        (setf depth (+ depth 1))
    )
)

(defun look (a b depth)
  (setf x 0)
    (loop while (>= x depth)

          )
)
