(defstruct Node 
  parent
  typeNode ;; 2-node or 3-node
  leftChild
  rightChild
  middleChild
  leftVal
  rightVal
)

(defun newNode ()
  (make-Node)
)

(defun newTwoNode (value)
  (make-Node :leftVal value :typeNode 'twoNode)
)

(defun newThreeNode (leftVal rightVal)
  (make-Node :leftVal leftVal :rightVal rightVal :typeNode 'threeNode)
)

(defun setLeftChild (node leftChild)
    (setf (Node-leftChild node) leftChild)  
    (if (not (null leftChild))
      (setf (Node-parent leftChild) node)
      )
)

(defun getLeftChild (node)
  (Node-leftChild node)
  )

(defun setRightChild (node rightChild)
  (setf (Node-rightChild node) rightChild)
  (if (not (null rightChild))
    (setf (Node-parent rightChild) node)
    )
)

(defun getRightChild (node)
  (Node-rightChild node)
  )

(defun removeChildren (node)
  (setf (Node-leftChild node) nil)
  (setf (Node-rightChild node) nil)
)

(defun setMiddleChild (node middleChild)
  (if (equal 'threeNode (Node-typeNode node))
    (lambda (setf (Node-middleChild node) middleChild)
      (if (not (null middleChild))
        (setf (Node-parent middleChild) node)
     )
    )
    )
)

(defun getParent (node)
  (Node-parent node)
)

(defun setParent (node parent)
  (setf (Node-parent node) parent)
)

(defun getValue (node)
    (Node-leftVal node)
)

(defun getRightVal (node)
  (Node-rightVal node)
)

(defun isTwoNode (node)
  (if (equal 'twoNode (Node-typeNode node))
    (return 'T)
    (return 'null)
    )
  )

(defun isThreeNode (node)
  (return (not (isTwoNode node)))
)

(defun isTerminal (node)
  (return-from isTerminal 
               (and (null (getLeftChild node))
                    (null (getRightChild node))))
)

(defun replaceChild (node currentChild newChild)
    (if (equalp currentChild newChild)
      (setLeftChild node newChild)
      (if (equalp currentChild (getRightChild node))
        (setRightChild node newChild)
        (if (not (equalp (getMiddleChild node) currentChild))
          (setMiddleChild node newChild)
          )
        )
      )
    (setParent newChild node)
    (setParent currentChild null)
  )



(defparameter root nil)
(defparameter size 0)

(defun addToThree (value)
    (if (null root)
      (setf root (newTwoNode value))
      (progn
       (setf result (insert value root))
       (print result)
       (if (not (null result))
         (setf root result)
       )
      )
    )
    (setf size (+ size 1))
    T
)

(defun contains (value)
  (return (not (null (findNode node value))))
)

(defun compareTo (nodeA nodeB)

  (if (equalp nodeA nodeB) (return-from compareTo 0))
  (if (and (numberp nodeA) (numberp nodeB))
    (if (< nodeA nodeB) (return-from compareTo -1) (return-from compareTo 1))
    (if (string< nodeA nodeB) (return-from compareTo -1) (return-from compareTo 1))
    )
)

(defun findNode (node value)
  (if (null node) (return-from findNode 'nil))

  (if (equal 'threeNode (Node-typeNode node))
    (progn 
      (setf leftComp (compareTo value (getValue (getLeftNode node))))
      (setf rightComp (compareTo value (getValue (getRightNode node))))
      (if (and (= leftComp 0) (= rightComp 0))
        (return node)
        )
      (if (< leftComp 0)
         (return (findNode (getLeftChild node) value))
         (if (< rightComp 0)
           (return (findNode (getMiddleChild node) value))
           (return (findNode (getRightChild node) value))
           )
         )
        )
    (progn
      (setf comp (compareTo value (getValue node)))
      (if (= comp 0)
        (return-from findNode node)
        (if (< comp 0)
          (return-from findNode node (findNode (getLeftChild node) value))
          (return-from findNode node (findNode (getRightChild node) value))
          )
        )
    )
  )
)

(defun unlinkNode (node)
  (removeChildren node)
  (setParent node nil)
)

(defun insert (value node)
  
  (if (equal 'twoNode (Node-typeNode node))
    (progn
        (setf comp (compareTo value (getValue node)))
        
        (if (isTerminal node)
          (progn 
            (if (equal comp 0)
                (return-from insert 'null))
            (setf thnode (newThreeNode value (getValue node)))
            (setf parent (getParent node))
            (if (not (null parent))
                (replaceChild parent node thnode)
                (setf root thnode)
            )
          )
          (progn 
            (if (< comp 0)
              (progn 
                (setf result (insert value (getLeftChild node)))
                (if (not (null result))
                  (progn
                    (setf threeNode (newThreeNode (getValue result) (getValue node)))
                    (setRightChild threeNode (getRightChild node))
                    (setMiddleChild threeNode (getRightChild result))
                    (setLeftChild threeNode (getLeftChild result))
                    (if (not (null (getParent node)))
                      (replaceChild (getParent node) node threeNode)
                      (setf root threeNode)
                      )
                    (unlinkNode node)
                    )
                  )
                )
                (if (> comp 0)
                  (progn
                    (setf result (insert value (getRightChild node)))
                    (if (not (null result))
                        (progn
                            (setf threeNode (newThreeNode (getValue result) (getValue node)))
                            (setLeftChild threeNode (getLeftChild node))
                            (setMiddleChild threeNode (getLeftChild result))
                            (setRightChild threeNode (getRightChild result))
                            (if (not (null (getParent node)))
                              (replaceChild (getParent node) node threeNode)
                              (setf root threeNode)
                              )
                            (unlinkNode node)
                            )
                        )
                    )
                    (return-from insert 'null)
                  ) 
                )
            )
        )
      )
    (progn  ;; three node
        (setf threeNode node)

        (setf leftComp (compareTo value (getValue threeNode)))
        (setf rightComp (compareTo value (getRightVal threeNode)))
        (print leftComp)
        (print rightComp)
        (if (or (= leftComp 0) (= rightComp 0))
          (return-from insert 'null)
        )

        (if (isTerminal threeNode)
          (setf returnValue (splitNode threeNode value))
          (progn
            (if (< leftComp 0)
              (progn
                (setf result (insert value (getLeftChild threeNode)))
                (if (not (null result))
                  (progn
                    (setf returnValue (splitNode threeNode (getValue result)))
                    (setLeftChild (getLeftChild returnValue) (getLeftChild result))
                    (setRightChild (getLeftChild returnValue) (getRightChild result))
                    (setLeftChild (getRightChild returnValue) (getMiddleChild threeNode))
                    (setRightChild (getRightChild returnValue) (getRightChild threeNode))
                    (unlinkNode threeNode)
                  )
                )
               )
               (if (< rightComp 0)
                 (progn
                   (setf result (insert value (getMiddleChild threeNode)))
                   (print result)
                   (if (not (null result))
                     (progn
                       (setf returnValue (splitNode threeNode (getValue result)))
                       (setLeftChild (getLeftChild returnValue) (getLeftChild threeNode))
                       (setRightChild (getLeftChild returnValue) (getLeftChild result))
                       (setLeftChild (getRightChild returnValue) (getRightChild result))
                       (setRightChild (getRightChild returnValue) (getRightChild threeNode))
                       (unlinkNode threeNode)
                     )
                     (print "is null")
                  )
                  (progn
                    (setf result (insert value (getRightChild threeNode)))
                    (if (not (null result))
                      (progn
                        (setf returnValue (splitNode threeNode (getValue result)))
                        (setLeftChild (getLeftChild returnValue) (getLeftChild threeNode))
                        (setRightChild (getLeftChild returnValue) (getMiddleNode threeNode))
                        (setLeftChild (getRightChild returnValue) (getLeftChild result))
                        (setRightChild (getRightChild returnValue) (getRightChild result))
                        (unlinkNode threeNode)
                      )
                    )
                    )
                  )
                 )
               )
            )
          )
        )
    )
    (return-from insert returnValue)
  )


(defun splitNode (threeNode value)
  (if (< (compareTo value (getValue threeNode)) 0)
    (progn 
      (setf mini value)
      (setf middle (getValue threeNode))
      (setf maxi (getRightVal threeNode))
      )
    (if (< (compareTo value (getRightVal threeNode)) 0)
      (progn
        (setf mini (getValue threeNode))
        (setf middle value)
        (setf maxi (getRightVal threeNode))
        )
      (progn
        (setf mini (getValue threeNode))
        (setf maxi value)
        (setf middle (getRightVal threeNode))
        )
      )
    )
  (setf parent (newTwoNode middle))
  (setLeftChild parent (newTwoNode mini))
  (setRightChild parent (newTwoNode maxi))
  (return-from splitNode parent)
)
