



(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))



(defun is-graph (g)
  (gethash g *graphs*))



(defun new-graph (g)
  (or (gethash g *graphs*)
      (setf (gethash g *graphs*) g)))



(defun delete-graph (g)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) g) (remhash k *vertices*))))
           *vertices*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) g) (remhash k *arcs*))))
           *arcs*))



(defun new-vertex (g v)
  (setf (gethash (list 'vertex g v)
                 *vertices*)
        (list 'vertex g v)))



;; Returns a list containing all the vertices in a given graph rapresented by g

(defun graph-vertices (g)
  (let ((acc '()))
  (maphash #'(lambda (key val)
               (cond ((equal (second val) g)
                      (setq acc (append acc (list val))))))
           *vertices*)
  acc))



;; The function new-arc creates an entry in the hash table rapresenting a new
;; arc between two vertices using the following notation:
;; (arc g v-destination v-source weight)
;; Note that the parameter rapresenting the arc's weight is optional, its
;; default value is set to 1.

(defun new-arc (g u v &optional (w 1))
  (setf (gethash (list 'arc g v w)
                 *arcs*)
        (list 'arc g v w)))


(defun graph-arcs (g)
  (let ((acc '()))
    (maphash #'(lambda (key val)
                 (cond ((equal (second val)) g)
                       (setq acc (append acc (list val)))))
             *arcs*)
    acc))





;; DEBUG ONLY REMOVE BEFORE RELEASE
(new-graph 'greg)

(new-vertex 'greg 'a)
(new-vertex 'greg 'b)
(new-vertex 'greg 'c)
(new-vertex 'greg 'd)
(new-vertex 'greg 'e)
(new-vertex 'greg 'f)
(new-vertex 'greg 'g)
(new-vertex 'greg 'h)
(new-vertex 'greg 'i)

(new-arc 'greg 'b 'a 4)
(new-arc 'greg 'h 'a 8)
(new-arc 'greg 'b 'h 11)
(new-arc 'greg 'i 'h 7)
(new-arc 'greg 'g 'h 1)
(new-arc 'greg 'c 'b 8)
(new-arc 'greg 'c 'i 2)
(new-arc 'greg 'g 'i 6)
(new-arc 'greg 'f 'g 2)
(new-arc 'greg 'f 'c 4)
(new-arc 'greg 'e 'f 10)
(new-arc 'greg 'd 'f 14)
(new-arc 'greg 'd 'c 7)
(new-arc 'greg 'e 'd )
;; DEBUG ONLY REMOVE BEFORE RELEASE
