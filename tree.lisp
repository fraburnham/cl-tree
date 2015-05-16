(defpackage :cl-tree
  (:use :common-lisp))

;; some notes on trees
;; a tree is a list of nodes
;; a node is a list whose first is the data and rest are children (as a tree)
;; tree:
;; ((node (child (grandchild))) (node2 (child (grandchild))))
;; node:
;; (node (child (grandchild)))

(defvar tree '((node-root
                (node-depth-1 (node-depth-2 (node-depth-3)))
                (node-depth-1 (node-depth-2))
                (node-depth-1)
                (node-depth-1 (node-depth-2)))
               (node-root-2
                (child-node-1 (child-node-1-2))
                (child-node-2))))

(defun print-tabs (tabiness)
  "Print out tabs to make dfp prettier"
  (cond ((> tabiness 0)
         (format t "~T")
         (print-tabs (- tabiness 1)))))

(defun dfp (tree &optional (tabiness 0))
  "Depth first print of the tree"
  (let ((node (first tree))
        (node-data (first (first tree))))
    (cond ((null tree) nil)
          (t
           (print-tabs tabiness)
           (format t "~a~%" node-data)
           (if (rest node)
               (dfp (rest node) (+ 1 tabiness)))))
    (if (rest tree)
        (dfp (rest tree) tabiness))))

(defun dfs (tree search-data)
  "Depth first search of tree looking for search-data"
  (let ((node (first tree))
        (node-data (first (first tree))))
    (let ((found
           (cond ((null tree) nil)
                 ((equal node-data search-data) node)
                 (t
                  (if (rest node) ;search down this node until you hit the bottom
                      (dfs (rest node) search-data))))))
      (if (and (null found) ;didn't find the search-data
               (rest tree)) ;and the tree is not empty (just the path down the node was)
          (dfs (rest tree) search-data) ;so search the rest of the tree
          found)))) ;or return what you've found

(defun ->tree (node)
  "Convert a node into a tree"
  (list node))

(defun ->node (tree)
  "Convert a tree into a node"
  (first tree))

(defun update-children (node child)
  "Add a child to the node"
  (let ((children (rest node))
        (node-data (first node)))
    (cons node-data (cons child children))))

(defun add-child (tree parent-data child)
  "Add a child to the given node (found by its data)
   on the given tree"
  ;; find the node, update the children, rebuild the tree
  (let ((node (first tree))
        (node-data (first (first tree))))
    ;; the below finds the node and updates the child, now wrap it to
    ;; rebuild the tree.
    (cond ((equal node-data parent-data) (update-children node child))
          ((null tree) nil)
          (t
           (if (rest node)
               (add-child tree parent-data child))))
    )
  )
