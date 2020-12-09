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



(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))



(defun is-graph (graph-id)
  (gethash graph-id *graphs*))



(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))



(defun delete-graph (graph-id)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) graph-id) (remhash k *vertices*))))
           *vertices*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) graph-id) (remhash k *arcs*))))
           *arcs*))



(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id)
                 *vertices*)
        (list 'vertex graph-id vertex-id)))



;; Returns a list containing all the vertices in a given graph rapresented by
;; its graph-id

(defun temp-graph-vertices (graph-id)
  (setf 'acc ())
  (maphash #'(lambda (k v)
               (cond (equal (second v) graph-id)
                     (setf 'acc (append (gethash 'acc) v))))
           *vertices*)
  (gethash 'acc))


(defun graph-vertexes (graph-id)
  (let ((acc '()))
    (maphash #'(lambda (k v)
                 (cond ((equal (second v) graph-id)
                        (setq acc (append acc v)))))
             *vertices*)
    (acc)))



;; TEMP DEMO
;; What graph-vertices should do.

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))



;; The function new-arc creates an entry in the hash table rapresenting a new
;; arc between two vertices using the following notation:
;; (arc graph-id vertex-id-destination vertex-id-source weight)
;; Note that the parameter rapresenting the arc's weight is optional, its
;; default value is set to 1.

(defun new-arc (graph-id vertex-id-dest vertex-id-source &optional (weight 1))
  (setf (gethash (list 'arc graph-id vertex-id-dest vertex-id-source weight)
                 *arcs*)
        (list 'arc graph-id vertex-id-dest vertex-id-source weight)))
