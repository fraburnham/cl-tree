(defpackage :cl-tree
  (:use :common-lisp))

(defvar shallow-tree '((node-root
                        (node-depth-1 (node-depth-2))
                        (node-depth-1 (node-depth-2))
                        (node-depth-1)
                        (node-depth-1 (node-depth-2)))
                       (node-root-2
                        (child-node-1 (child-node-1-2))
                        (child-node-2))
                       leaf-root))

(defun maptree (wind-fn unwind-fn tree)
  (when tree
    (let ((node (first tree)))
      (let ((r (funcall wind-fn node)))
        (funcall unwind-fn r)))))

(defun println (data)
  (format t "~a~%" data))

(defun print-tabs (tabiness)
  "Print out tabs to make dfp prettier"
  (cond ((> tabiness 0)
         (format t "~T")
         (print-tabs (1- tabiness)))))

(defun leafp (node)
  "Return true if the node is a leaf"
  (not (listp node)))

(defun branchp (node)
  "Return true if the node is a branch"
  (listp node))

(defun node-equalp (node data &key (equal-fn #'equal))
  (cond ((branchp node)
         (funcall equal-fn (first node) data))
        ((leafp node)
         (funcall equal-fn node data))))

;; this is an awful name.
(defun node-identity (node new-data)
  "Return new-data.
  Takes the same args that will be given to any fn passed as :update-fn to update-node."
  new-data)

(defun dfprint (tree &optional (tabiness 0))
  (maptree (lambda (node)
             (print-tabs tabiness)
             (cond ((branchp node)
                    (println (first node))
                    (cons (first node) (map-print (rest node) (1+ tabiness))))
                   (t
                    (println node)
                    node)))
           (lambda (node)
             (cons node (map-print (rest tree) tabiness)))
           tree))

(defun replace-all (tree search-data new-data)
  (maptree (lambda (node)
             (cond ((equal search-data node) new-data)
                   ((branchp node)
                    (if (equal search-data (first node))
                        (cons new-data (rest node))
                        (cons (first node) (replace-all (rest node) search-data new-data))))
                   (t node)))
           (lambda (node)
             (cons node (replace-all (rest tree) search-data new-data)))
           tree))

(defun replace-first (tree search-data new-data)
  (maptree ;; maybe replace first is a reduce
   ;; build the new tree and stop replacing once one has been replaced
   ;; try writing it this way to see if it clarifies anything in the process
   ;; for me. I'll need to know trees very well to work on gp. Make the trees make some sense!
   (lambda (node)
     (cond ((equal search-data node) (list new-data t))
           ((branchp node)
            (if (equal search-data (first node))
                (list (cons new-data (rest node)) t) ; the node has been found!
                (let ((r (replace-first (rest node) search-data new-data)))
                  (let ((node (first r))
                        (found (second r)))
                    (list (cons (first node) (replace-first (rest node) search-data new-data)) found)))))
           (t (list () nil))))
   (lambda (node)
     (let ((node (first node))
           (found (second node)))
       (if found
           (cons node (rest tree)) ;; found the node, stop searching
           ;; the problem is in the code below? it's in the unwind somewhere I think
           (cons node (replace-first (rest tree) search-data new-data)))))
   tree))
