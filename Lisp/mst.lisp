(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter default-heap-size 42)



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
                        (push val acc))))
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
                        (push val acc))))
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
;; How at least one of the two neighbors function should look like.
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



;;;; MINHEAP IMPLEMENTATION


;;; Create a new heap in the hashtable *heaps*.
;;; ---> heap-rep: (HEAP heap-id heap-size actual-heap)
(defun new-heap (heap-id &optional (capacity default-heap-size))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity
                                              :initial-element nil
                                              :fill-pointer 0
                                              )))))



;;; Access function for a heap-rep.
(defun heap-id (heap-rep)
  (cond ((equal (first heap-rep) 'heap)
         (second heap-id))
        (T nil)))



;;; Access function for a heap-rep.
;;; Return the number of elements in the heap NOT the actual array dimension.
(defun heap-size (heap-rep)
  (cond ((equal (first heap-rep) 'heap)
         (third heap-rep))
        (T nil)))



;;; Access function for a heap-rep.
;;; Return the actual array representing the heap.
(defun heap-actual-heap (heap-rep)
  (cond ((equal (first heap-rep) 'heap)
         (fourth heap-rep))
        (T nil)))



(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))



(defun heap-empty (heap-id)
  (cond ((zerop (third (gethash heap-id *heaps*))) T)
        (T nil)))



(defun heap-not-empty (heap-id)
  (cond ((equal (heap-empty heap-id) nil))
        (T nil)))



;;; Return a (list K V) where K is the minimum key in the heap and V the
;;; associated value.
(defun heap-head (heap-id)
  (aref (fourth (gethash heap-id *heaps*)) 0))



;;; Add an element to the heap while maintaining the min heap property.
;;;
;;; Min-Heap-Insert(A, Key)
;;;     A.heap-size = A.heap-size + 1
;;;     A[A.heap-size] = -inf
;;;     Heap-Decrease-Key(A, A.heap-size, key)
;;;
;;; TODO: check out of bounds before updating (third heap-rep)
(defun heap-insert (heap-id k v)
  (setf (third (gethash heap-id *heaps*))
        (+ (third (gethash heap-id *heaps*)) 1))
  (let ((heap-actual-heap (fourth (gethash heap-id *heaps*))))))



;;; HEAP DECREASE KEY
;;;
;;; heap-decrease-key(A, i, key)
;;;     if key < A[i]
;;;         error "the new key is less than the current one"
;;;     A[i] = key
;;;     while i > 1 AND A[Parent(i)] < A[i]
;;;         switch A[i] with A[Parent(i)]
;;;         i = Parent(i)
;;;
(defun heap-descrease-key (heap-id i k))





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


(new-heap 'hip 20)

(setf (aref (fourth (gethash 'hip *heaps*)) 0) (list '1 'uno))
(setf (aref (fourth (gethash 'hip *heaps*)) 1) (list '8 'otto))
(setf (aref (fourth (gethash 'hip *heaps*)) 2) (list '2 'due))
(setf (aref (fourth (gethash 'hip *heaps*)) 3) (list '3 'tre))
(setf (aref (fourth (gethash 'hip *heaps*)) 4) (list '7 'sett))
(setf (aref (fourth (gethash 'hip *heaps*)) 5) (list 'K6 'V6))
(setf (aref (fourth (gethash 'hip *heaps*)) 6) (list 'K7 'V7))
(setf (aref (fourth (gethash 'hip *heaps*)) 7) (list 'K8 'V8))
(setf (aref (fourth (gethash 'hip *heaps*)) 8) (list 'K9 'V9))
(setf (aref (fourth (gethash 'hip *heaps*)) 9) (list 'K9 'V9))
