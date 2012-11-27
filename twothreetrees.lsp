;;;
 ;; File: twotreetrees
 ;; Author: Lourens Schep, 100103961
 ;; Date:   November 27,2012
 ;; For:    Project, COMP 3613, Fall, 2012
;;;

;;; (defstruct Node)
 ;; Is the struct used for the tree which 
 ;; is the actual node the struct contains
 ;; info for either a single/twonode/three node
 ;; the main value for the node is always put into
 ;; the leftVal.
 ;; parent: is the parent of the node
 ;; typeNode: is the type of the node
;;;
(defstruct Node 
  parent
  typeNode ;; 2-node or 3-node
  leftChild
  rightChild
  middleChild
  leftVal
  rightVal
)

;;; (newTwoNode value)
 ;; -- value is the value the new Node
 ;; contains which is set to the leftVal.
 ;; return the new two node made.
;;; 
(defun newTwoNode (value)
  (make-Node :leftVal value :typeNode 'twoNode)
)

;;; (newThreeNode leftVal rightVal)
 ;; -- leftVal is the value that goes to the left 
 ;; in the new three node
 ;; -- rightVal is the value that goes to the right
 ;; in the new three node
 ;; The function return the new three node 
 ;; it also assigns the smaller value on the left
 ;; and the bigger on the right.
;;; 
(defun newThreeNode (leftVal rightVal)
  (setf comp (compareTo leftVal rightVal))
  (if (< comp 0)
    (make-Node :leftVal leftVal :rightVal rightVal :typeNode 'threeNode)
    (make-Node :leftVal rightVal :rightVal leftVal :typeNode 'threeNode)
    )
)

;;; (setLeftChild node leftChild)
 ;; -- node is the node it uses to assign
 ;; the left child
 ;; -- leftChild is the left child it assigns
 ;; to the leftChild value in the given node.
;;;
(defun setLeftChild (node leftChild)
    (setf (Node-leftChild node) leftChild)  
    (if (not (null leftChild))
      (setf (Node-parent leftChild) node)
      ))

;;; (getLeftChild node)
 ;; -- node is the node it uses to return leftchild
 ;; it returns the left child of node
;;; 
(defun getLeftChild (node)
  (Node-leftChild node)
  )

;;; (setRightChild node rightChild)
 ;; -- node is the node it uses
 ;; -- rightChild is the node that is the 
 ;; right child of the given node
;;;
(defun setRightChild (node rightChild)
  (setf (Node-rightChild node) rightChild)
  (if (not (null rightChild))
    (setf (Node-parent rightChild) node)
    )
)

;;; (getRightChild node)
 ;; -- node is the node it uses
 ;; it returns the rightchild of the node
;;; 
(defun getRightChild (node)
  (Node-rightChild node)
  )

;;; (removeChildren node)
 ;; -- node is the node it uses
 ;; removes children of the given node
;;; 
(defun removeChildren (node)
  (setf (Node-leftChild node) nil)
  (setf (Node-rightChild node) nil)
)

;;; (setMiddleChild node middleChild)
 ;; -- node is the node it uses
 ;; -- middleChild is the node it assigns
 ;; the middleChild value to in node
;;; 
(defun setMiddleChild (node middleChild)
  (if (equal 'threeNode (Node-typeNode node))
    (progn (setf (Node-middleChild node) middleChild)
      (if (not (null middleChild))
        (setf (Node-parent middleChild) node)
     )
    )
    )
)

;;; (getMiddleChild node)
 ;; -- node is the node it uses
 ;; it returns the middle child of the given node
;;; 
(defun getMiddleChild (node)
  (Node-middleChild node))

;;; (getParent node)
 ;; -- node is the node it uses
 ;; returns the parent of the given node.
;;; 
(defun getParent (node)
  (Node-parent node)
)

;;; (setParent node parent)
 ;; -- node is the node it uses
 ;; -- parent is new parent for node
 ;; it assigns the parent node to the
 ;; parent value in node.
;;; 
(defun setParent (node parent)
  (setf (Node-parent node) parent)
)

;;; (getValue node)
 ;; -- node is the node it uses
 ;; returns the value of the node which is
 ;; located in the leftvalue of the node.
;;; 
(defun getValue (node)
    (Node-leftVal node)
)

;;; (getRightVal node)
 ;; -- node is the node it uses
 ;; Returns the right value of the given node
 ;; assuming the given node is a three node.
;;; 
(defun getRightVal (node)
  (Node-rightVal node)
)

;;; (isTwoNode node)
 ;; -- node is the node it uses
 ;; Returns T if the node is a two node
 ;; otherwise it returns nil
;;; 
(defun isTwoNode (node)
  (if (equal 'twoNode (Node-typeNode node))
    (return-from isTwoNode 'T)
    (return-from isTwoNode 'nil)
    )
  )

;;; (isThreeNode node)
 ;; -- node is the node it uses
 ;; returns T if the node is a three node
 ;; otherwise it returns nil
 ;; it uses isTwoNode since you can only have one
 ;; or the other.
;;; 
(defun isThreeNode (node)
  (return-from isThreeNode (not (isTwoNode node)))
)

;;; (isLeave node)
 ;; -- node is the node it uses
 ;; returns if the given node is a leave
 ;; meaning it does not contain either a 
 ;; leftchild or a rightchild.
;;; 
(defun isLeave (node)
  (return-from isLeave 
               (and (null (getLeftChild node))
                    (null (getRightChild node))))
)

;;; (replaceChild node currentChild newChild)
 ;; -- node is the node it uses.
 ;; -- currentChild is the child currently used
 ;; in the node.
 ;; -- newChild is the new child that currentChild
 ;; is going to be replaced with.
 ;; it replaces the currentChild in node with the 
 ;; newchild
;;; 
(defun replaceChild (node currentChild newChild)
  (debugMode "called replace Child")
  (if (equalp currentChild (getLeftChild node))
      (setLeftChild node newChild)
      (if (equalp currentChild (getRightChild node))
        (setRightChild node newChild)
        (if (equalp (getMiddleChild node) currentChild)
          (setMiddleChild node newChild)
          )
        )
      )
    (setParent newChild node)
    (setParent currentChild 'nil)
    T
  )

;;; (initiateTree)
 ;; Assigns the root of the tree root
 ;; to nil for starting a new tree
 ;; root is a global variable used in all the
 ;; functions.
;;; 
(defun initiateTree ()
    (defparameter root 'nil)
    (defparameter nodesAdded '())
)

;;; (addToTree value)
 ;; -- value is the value to be added to the tree.
 ;; Adds the given value to the tree using
 ;; the addHelper function.
;;; 
(defun addToTree (value)
    (if (null root)
      (setf root (newTwoNode value))
      (progn
       (setf result (addHelper value root))
       (if (not (null result))
         (setf root result)
       )
      )
    )
    (debugMode (format nil "Added node: ~A ~%" value))
    (setf nodesAdded (append nodesAdded (list value)))
    T
)

;;; (compareTo nodeA nodeB)
 ;; -- nodeA is one of the nodes being compared
 ;; -- nodeB is the other node being compared
 ;; It returns either -1, 0 or 1 which is the same as 
 ;; a compare function in java it will compare either integers
 ;; or strings so that you can make your tree either a integer
 ;; or string two/three tree.
;;; 
(defun compareTo (nodeA nodeB)

  (if (equalp nodeA nodeB) (return-from compareTo 0))
  (if (and (numberp nodeA) (numberp nodeB))
    (if (< nodeA nodeB) (return-from compareTo -1) (return-from compareTo 1))
    (if (string< nodeA nodeB) (return-from compareTo -1) (return-from compareTo 1))
    )
)

;;; (detachNode node)
 ;; -- node is the node being detached from tree
 ;; basically removes the node by making it unreachable.
;;; 
(defun detachNode (node)
  (removeChildren node)
  (setParent node nil)
)


;;; (addHelper value node)
 ;; -- value is the value being added.
 ;; -- node is the root of the branch its looking at.
 ;; Is function is used by addToTree to add the value to the 
 ;; tree and also where all the functionality of the two/three
 ;; tree happens.
 ;; it uses recursion so add helper gets called multiple times 
 ;; with adding one value mostly dependent on how deep the tree is.
;;; 
(defun addHelper (value node)
  (debugMode (format nil "~% Insert ~A Node ~A ~%" value (getValue node)))
  (setf returnValue 'nil) 
  (if (equal 'twoNode (Node-typeNode node)) ;; if node is a two node 
    (progn
        (setf comp (compareTo value (getValue node)))
        (debugMode (format nil "TwoNode comp: ~A " comp))
        (if (isLeave node)
          (progn
            (debugMode "isleave node")
            (if (equal comp 0)
                (return-from addHelper 'nil))
            (setf thnode (newThreeNode value (getValue node)))
            (setf parent (getParent node))
            (if (not (null parent))
                (replaceChild parent node thnode)
                (setf root thnode)
            )
          )
          (progn
            (debugMode "is not leave node")
            (if (< comp 0)
              (progn 
                (setf result (addHelper value (getLeftChild node)))
                (setf returnValue 'nil)
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
                    (detachNode node)
                    )
                  )
                )
                (if (> comp 0)
                  (progn
                    (setf result (addHelper value (getRightChild node)))
                    (setf returnValue 'nil)
                    (debugMode "Came back from insert ~%")
                    (if (not (null result))
                        (progn
                            (setf threeNode (newThreeNode (getValue result) (getValue node)))
                            (setLeftChild threeNode (getLeftChild node))
                            (setMiddleChild threeNode (getLeftChild result))
                            (setRightChild threeNode (getRightChild result))
                            (debugMode (format nil "threeNode ~A ~A ~%" (getValue threeNode) (getRightVal threeNode)))
                            (if (not (null (getParent node)))
                              (replaceChild (getParent node) node threeNode)
                             (setf root threeNode)
                              )
                            (detachNode node)
                            (debugMode (format nil "Root: ~A ~A ~%" (getValue root) (getRightVal root)))
                            )
                        )
                    )
                    (return-from addHelper 'nil)
                  ) 
                )
            )
        )
      )
    (progn  ;; if node is a three node
      (setf threeNode node)
      (debugMode  "is three node")
        (setf leftComp (compareTo value (getValue threeNode)))
        (setf rightComp (compareTo value (getRightVal threeNode)))
        (debugMode (format nil "Left:~A Right:~A~%" leftComp rightComp))
        (if (or (= leftComp 0) (= rightComp 0))
          (return-from addHelper 'nil)
        )

        (if (isLeave threeNode)
          (setf returnValue (convertThreeNode threeNode value))
          (progn
            (debugMode  "not a leave")
            (if (< leftComp 0)
              (progn
                (setf result (addHelper value (getLeftChild threeNode)))
                (setf threeNode node)
                (if (not (null result))
                  (progn
                    (setf returnValue (convertThreeNode threeNode (getValue result)))
                    (setLeftChild (getLeftChild returnValue) (getLeftChild result))
                    (setRightChild (getLeftChild returnValue) (getRightChild result))
                    (setLeftChild (getRightChild returnValue) (getMiddleChild threeNode))
                    (setRightChild (getRightChild returnValue) (getRightChild threeNode))
                    (detachNode threeNode)
                  )
                  (return-from addHelper 'nil)
                )
               )
               (if (< rightComp 0)
                 (progn
                   (setf result (addHelper value (getMiddleChild threeNode)))
                   (setf threeNode node)
                   (if (not (null result))
                     (progn
                       (setf returnValue (convertThreeNode threeNode (getValue result)))
                       (setLeftChild (getLeftChild returnValue) (getLeftChild threeNode))
                       (setRightChild (getLeftChild returnValue) (getLeftChild result))
                       (setLeftChild (getRightChild returnValue) (getRightChild result))
                       (setRightChild (getRightChild returnValue) (getRightChild threeNode))
                       (detachNode threeNode)
                     )
                     (debugMode "is null"))
                  )
                  (progn
                    (setf result (addHelper value (getRightChild threeNode)))
                    (setf threeNode node)
                    (if (not (null result))
                      (progn
                        (setf returnValue (convertThreeNode threeNode (getValue result)))
                        (setLeftChild (getLeftChild returnValue) (getLeftChild threeNode))
                        (setRightChild (getLeftChild returnValue) (getMiddleChild threeNode))
                        (setLeftChild (getRightChild returnValue) (getLeftChild result))
                        (setRightChild (getRightChild returnValue) (getRightChild result))
                        (detachNode threeNode)
                      )
                    )
                  )
                 )
               )
            )
          )
        )
    )
    (return-from addHelper returnValue)
  )


;;; (convertThreeNode threeNode value)
 ;; -- threeNode is the node that will be converted
 ;; -- value is the value used to convert it.
 ;; convertThreeNode gets a three node and a value
 ;; from that it makes two new twonodes and a parent twonode
 ;; so it makes a tree out of it and returns the parent.
;;;
(defun convertThreeNode (threeNode value)
  (setf compLeftVal (compareTo value (getValue threeNode)))
  (setf compRightVal (compareTo value (getRightVal threeNode)))
  (if (< compLeftVal 0)
    (progn 
      (setf mini value)
      (setf middle (getValue threeNode))
      (setf maxi (getRightVal threeNode))
      )
    (if (< compRightVal 0)
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
  (debugMode (format nil "Splitnode: ~A ~A ~A ~%" mini middle maxi))
  (return-from convertThreeNode parent)
)


;;; (removeNode value)
 ;; -- value is the value being removed
 ;; removeNode removes the current value from the 
 ;; two/three tree by making a new one without that value.
;;;
(defun removeNode (value)
  (setf nodes nodesAdded)
  (initiateTree)
  (loop while (> (length nodes) 0) do
    (setf n (first nodes))
    (setf nodes (rest nodes))
    (if (not (equal n value))
      (addToTree n)
        )
    )
)

;;; (printTree topNode)
 ;; -- topNode is the root of the tree
 ;; Prints out the given tree.
;;; 
(defun printTree (topNode)
  (setf nodes (list topNode 'nil))
  (loop while (> (length nodes) 1) do
        (setf n (first nodes))
        (setf nodes (rest nodes))
        (cond
          ((null n)
           (progn
             (format t "~%")
             (setf nodes (append nodes (list '())))))
          ((isTwoNode n)
           (progn 
             (format t " (~A) " (getValue n))
             (unless (null (getLeftChild n))
               (setf nodes (append nodes (list (getLeftChild n)))))
             (unless (null (getRightChild n))
               (setf nodes (append nodes (list (getRightChild n)))))))
          ((isThreeNode n) 
           (progn
             (format t " (~A)(~A) " (getValue n) (getRightVal n))
             (unless (null (getLeftChild n))
               (setf nodes (append nodes (list (getLeftChild n)))))
             (unless (null (getMiddleChild n))
               (setf nodes (append nodes (list (getMiddleChild n)))))
             (unless (null (getRightChild n))
               (setf nodes (append nodes (list (getRightChild n)))))))))
  (format t "~%")
  )
 
(defparameter debugModeOn 'nil)
(defun debugMode (statement)
  (if (equal debugModeOn T)
    (print statement)
    )
)

;;; test function for script purposes
(defun test ()

  (initiateTree)
  (format t "Add a ~%")
  (addToTree 'a)
  (printTree root)
  (format t "Add l ~%")
  (addToTree 'l)
  (printTree root)
  (format t "Add g ~%")
  (addToTree 'g)
  (printTree root)
  (format t "Add o ~%")
  (addToTree 'o)
  (printTree root)
  (format t "Add r ~%")
  (addToTree 'r)
  (printTree root)
  (format t "Add i ~%")
  (addToTree 'i)
  (printTree root)
  (format t "Add t ~%")
  (addToTree 't)
  (printTree root)
  (format t "Add h ~%")
  (addToTree 'h)
  (printTree root)
  (format t "Add m ~%")
  (addToTree 'm)
  (printTree root)
  (format t "Add s ~%")
  (addToTree 's)
  (printTree root)
  (format t "Remove t ~%")
  (removeNode 't)
  (printTree root)

  (format t "Remove r,s,l,m ~%")
  (removeNode 'r)
  (removeNode 's)
  (removeNode 'l)
  (removeNode 'm)
  (printTree root)
)
