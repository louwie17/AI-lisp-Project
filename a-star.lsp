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

