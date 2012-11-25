(defstruct Node 
  parent
  typeNode ;; 2-node or 3-node
  leftChild
  rightChild
  middleChild
)

(defun newNode ()
  (return (make-Node))
)

(defun newTwoNode (node)
  (return (make-Node :leftChild node :typeNode 'twoNode))
)

(defun newThreeNode (leftVal rightVal)
  (return (make-Node :leftChild leftVal :rightChild rightVal :typeNode 'threeNode))
)

(defun setLeftChild (node leftChild)
    (setf (Node-leftChild node) leftChild)  
    (if (not (null leftChild))
      (setf (Node-parent leftChild) node)
      )
)

(defun setRightChild (node rightChild)
  (setf (Node-rightChild node) rightChild)
  (if (not (null rightChild))
    (setf (Node-parent rightChild) node)
    )
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
  (return (Node-parent node))
)

(defun setParent (node parent)
  (setf (Node-parent node) parent)
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

(defparameter root nil)
(defparameter size 0)

(defun addToThree (value)
    (if (null root)
      (setf root (newTwoNode value))
      (lambda
       (setf result (insert value root))
       (if (not (null result))
         (setf root result)
       )
      )
    )
    (setf size (+ size 1))
    (return 'T)
)

(defun contains (value)
  (return (not (null (findNode root value))))
)

(defun compareTo (nodeA nodeB)

  (if (equalp nodeA nodeB) (return 0))
  (if (and (numberp nodeA) (numberp nodeB))
    (if (< nodeA nodeB) (return -1) (return 1))
    (if (string< nodeA nodeB) (return -1) (return 1))
    )
)

(defun findNode (node value)
  (if (null node) (return null))

  (if (equal 'threeNode (Node-typeNode node))
    (lambda 
      (setf leftComp (compareTo value (getValue (getLeftNode node))))
      (setf rightComp (compareTo value (getValue (getRightNode node))))
      (if (and (= leftComp 0) (= rightComp 0))
        (return node)
        )
      (if (< leftComp 0)
        (
        )
    )
)

