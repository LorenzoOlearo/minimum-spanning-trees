(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))



(defun is-graph (g)
  (gethash g *graphs*))



;;; Create a new graph in the hashtable.
(defun new-graph (g)
  (or (gethash g *graphs*)
      (setf (gethash g *graphs*) g)))


;;; Delete the entire graph with all its arcs and vertices from the hash table.
(defun delete-graph (g)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) g) (remhash k *vertices*))))
           *vertices*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) g) (remhash k *arcs*))))
           *arcs*))



;;; Create a new vertex in the given graph.
(defun new-vertex (g v)
  (setf (gethash (list 'vertex g v)
                 *vertices*)
        (list 'vertex g v)))



;;; Return a list containing all the vertices in a given graph.
(defun graph-vertices (g)
  (let ((acc '()))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (cond ((equal (second val) g)
                        (setq acc (append acc (list val))))))
             *vertices*)
    acc))



;;; Create an entry in the hash table rapresenting a new arc between two
;;; vertices using the following notation:
;;; (arc g v-destination v-source weight)
;;; Note that the parameter rapresenting the arc's weight is optional and its
;;; default value is set to 1.
(defun new-arc (g u v &optional (w 1))
  (cond ((gethash (list 'arc g v u) *arcs*)
         (remhash (list 'arc g v u) *arcs*)))
  (setf (gethash (list 'arc g u v) *arcs*)
        (list 'arc g u v w)))



;;; Return a list containing all the arcs between two vertices in a given graph.
(defun graph-arcs (g)
  (let ((acc '()))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (cond ((equal (second val) g)
                        (setq acc (append acc (list val))))))
             *arcs*)
    acc))



;;; Return a list containing all the arcs connecting the the vertex v to its
;;; neighbors.
;;; The arcs elements of the list are rapresented with the following form:
;;; (arc graph-id vertex-id vertex-neighbor weight)
;;; Note that the implementation assumes a non oriented graph.
(defun graph-vertex-neighbors (g v)
  (remove nil
             (mapcar #'(lambda (arc)
                         (cond ((equal (third arc) v)
                                arc)
                               ((equal (fourth arc) v)
                                arc)
                               (T nil)))
                     (graph-arcs g))))



;; PLACEHOLDER while waiting for updated specifics.
;; What at least one of the two neighbors function should be.
(defun fixed-graph-vertices (g v)
  (remove nil
          (mapcar #'(lambda (arc)
                      (cond ((equal (third arc) v)
                             (list 'vertex g (fourth arc)))
                            ((equal (fourth arc) v)
                             (list 'vertex g (third arc)))
                            (T nil)))
                  (graph-arcs g))))



;;; Return a list containing all the vertices reachables from v, these
;;; connections are rapresented by arcs in the form:
;;; (arc graph-id vertex-id vertex-neighbor)
;;; Note that the implementation assumes a non oriented graph.
(defun graph-vertex-adjacent (g v)
  (remove nil
             (mapcar #'(lambda (arc)
                         (cond ((equal (third arc) v)
                                (remove-last arc))
                               ((equal (fourth arc) v)
                                (remove-last  arc))
                               (T nil)))
                     (graph-arcs g))))



;;; Support function for graph-vertex-adjacent.
;;; Remove the last element from a given list.
(defun remove-last (l)
  (reverse (cdr (reverse l))))



;;; Write the given graph in the standard output with the following notation:
;;; graph-id \n
;;; graph-vertices \n
;;; graph-arcs
(defun graph-print (g)
  (format t "~a~%~%" g)
  (format t "~{~a~%~}~%" (graph-vertices g))
  (format t "~{~a~%~}" (graph-arcs g)))



;;; DEBUG ONLY REMOVE BEFORE RELEASE
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
;;; DEBUG ONLY REMOVE BEFORE RELEASE
