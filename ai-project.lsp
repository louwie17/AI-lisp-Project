
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

(defun giveAllKeys ()
    (setf keys (list))
    (loop for key being the hash-keys of *nodes*
          using (hash-value value)
          do (append keys (list key)))
    (return keys)
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
;;; fill the table with the nodes and the struct as the list for the
;;; a star algorithm
(defstruct node
    attached
    g_score
    came_from
)

(defun makeStructs ()
    

)

(defun astar (start goal heuristic)
    (setf closedset ())
    (setf openset ()) ;; get all the nodes that are in the hashmap
    (makeStructs)
    (setf g_score 0)
    (setf f_score (+ g_score heuristic))

    (loop while (> (length openset) 0)
        (setf current) ;; having the lowest f_score value
        (if (equal current goal)
          (return (reconstruct_path))
          )

        (remove-if #'(lambda (x) (equal x current)) openset)
        (append closedset (list current))

        (loop for x in (getNodesConnected current) do
            (if (not (null (find x closedset)))
              (continue))
            ;; get the costs from the table
            (setf tentativeGscore (+ g_score (getcost current x)))
            (if (or (null (find x closedset)) (<= tentativeGscore (g_score x))) ;;; get from table
              (
               ;;; do something with the came_from ???
               ;;; g_score(x) = tentativeGscore
               ;;; f_score(neighbor) = g_score(neighbour) + heiristic cost estimate(neighboard, goal)
               (if (null (find x openset))
                 (append openset (list x))
               )
               )
              () ;;; else part
              )

        )
    )
    (return "Failure")
  
)

;;; reconstruct path function is a recursive function which only gets called when the
;;; a start algorithm reaches the goal and than it uses a recursive function to return
;;; the final fastest path.
