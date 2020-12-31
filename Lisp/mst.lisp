;;;; -*- Mode: Lisp -*-

;;;; mst.lisp --
;;;;
;;;; Minimum Spanning Trees
;;;; Progetto gennaio 2021 (E1P) Linguaggi di Programmazione Anno Accademico
;;;; 2020-2021
;;;;
;;;; Gruppo composto da:
;;;;    Lorenzo Olearo, matricola ------
;;;;    Alessandro Riva, matricola ------



(load "demo.lisp" :if-does-not-exist nil)

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))

(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter default-heap-size 42)

(defparameter *indices* (make-hash-table :test #'equal))

(defconstant inf most-positive-double-float)



;;;; GRAPHS IMPLEMENTATION


(defun is-graph (graph-id)
  (gethash graph-id *graphs*))



;;; Create a new graph in the hashtable.
(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))



;;; Delete the entire graph with all its arcs and vertices from the hash
;;; table.
(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) graph-id) (remhash k *vertices*))))
           *vertices*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) graph-id) (remhash k *arcs*))))
           *arcs*))



;;; Create a new vertex in the given graph.
(defun new-vertex (graph-id vertex-id)
  (cond ((is-graph graph-id)
         (setf (gethash (list 'vertex graph-id vertex-id)
                        *vertices*)
               (list 'vertex graph-id vertex-id)))
        (T nil)))



(defun has-vertex (graph-id vertex-id)
  (equal (second (gethash (list 'VERTEX graph-id vertex-id) *vertices*))
         graph-id))



;;; Return a list containing all the vertices in a given graph.
(defun graph-vertices (graph-id)
  (let ((acc '()))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (cond ((equal (second val) graph-id)
                        (push val acc))))
             *vertices*)
    acc))



;;; Create an entry in the hash table rapresenting a new arc between two
;;; vertices using the following notation:
;;; (arc g v-source v-dest weight)
;;; Note that the parameter rapresenting the arc's weight is optional and its
;;; default value is set to 1.
(defun new-arc (graph-id v u &optional (weight 1))
  (cond ((and (has-vertex graph-id u)
              (has-vertex graph-id v))
         (remhash (list 'arc graph-id u v) *arcs*)
         (setf (gethash (list 'arc graph-id v u) *arcs*)
               (list 'arc graph-id v u weight)))
        (T (error "UNKNOWN VERTICES"))))



;;; Return a list containing all the arcs between two vertices in a given
;;; graph.
(defun graph-arcs (graph-id)
  (let ((acc ()))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (cond ((equal (second val) graph-id)
                        (push val acc))))
             *arcs*)
    acc))



;;; Return a list containing all the arcs connecting the the vertex v to its
;;; neighbors.
;;; The arcs elements of the list are rapresented with the following form:
;;; (arc graph-id vertex-id vertex-neighbor weight)
;;; Note that the implementation assumes a non oriented graph.
(defun graph-vertex-neighbors (graph-id vertex-id)
  (remove nil
          (mapcar #'(lambda (u)
                      (or (gethash (list 'ARC
                                         graph-id
                                         vertex-id
                                         (third u))
                                   *arcs*)
                          (cond ((gethash (list 'ARC
                                                graph-id
                                                (third u)
                                                vertex-id)
                                          *arcs*)
                                 (list 'ARC
                                       graph-id
                                       vertex-id
                                       (third u)
                                       (fifth (gethash (list 'ARC
                                                             graph-id
                                                             (third u)
                                                             vertex-id)
                                                       *arcs*)))))))
                  (graph-vertices graph-id))))



;;; Return a list containing all the vertices reachables from v, these
;;; connections are rapresented by arcs in the form:
;;; (arc graph-id vertex-id vertex-neighbor)
;;; Note that the implementation assumes a non oriented graph.
(defun graph-vertex-adjacent (graph-id vertex-id)
  (remove nil
          (mapcar #'(lambda (u)
                      (cond ((or (gethash (list 'ARC
                                                graph-id
                                                vertex-id
                                                (third u))
                                          *arcs*)
                                 (gethash (list 'ARC
                                                graph-id
                                                (third u)
                                                vertex-id)
                                          *arcs*))
                             (list 'VERTEX graph-id (third u)))
                            (T nil)))
                  (graph-vertices graph-id))))



;;; Support function for graph-vertex-adjacent.
;;; Remove the last element from a given list.
(defun remove-last (l)
  (reverse (cdr (reverse l))))



;;; Write the given graph in the standard output with the following notation:
;;; graph-id \n
;;; graph-vertices \n
;;; graph-arcs
(defun graph-print (graph-id)
  (format t "~a~%~%" graph-id)
  (format t "~{~a~%~}~%" (graph-vertices graph-id))
  (format t "~{~a~%~}" (graph-arcs graph-id)))



;;;; MINHEAP IMPLEMENTATION


;;; Create a new heap in the hashtable *heaps*.
;;; ---> heap-rep: (HEAP heap-id heap-size actual-heap)
(defun new-heap (heap-id &optional (capacity default-heap-size))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity
                                              :initial-element nil)))))



;;; Access function for a heap-rep.
(defun heap-id (heap-rep)
  (cond ((equal (first heap-rep)
                'heap)
         (second heap-rep))
        (T nil)))



;;; Access function for a heap-rep.
;;; Return the number of elements in the heap NOT the actual array dimension.
(defun heap-size (heap-rep)
  (cond ((equal (first heap-rep)
                'heap)
         (third heap-rep))
        (T nil)))



;;; Access function for a heap-rep.
;;; Return the actual array representing the heap.
(defun heap-actual-heap (heap-rep)
  (cond ((equal (first heap-rep)
                'heap)
         (fourth heap-rep))
        (T nil)))



(defun heap-delete (heap-id)
  (cond ((gethash heap-id *heaps*)
         (remhash heap-id *heaps*))
        (T (error "NOT A HEAP"))))



(defun heap-empty (heap-id)
  (cond ((zerop (third (gethash heap-id *heaps*)))
         T)
        (T nil)))



(defun heap-not-empty (heap-id)
  (cond ((equal (heap-empty heap-id)
                nil))
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
;;;     Heap-Decrease-Key(A, (A.heap-size - 1), key)
;;;
(defun heap-insert (heap-id k v)
  (cond ((and (< (heap-size (gethash heap-id *heaps*))
                 (length (heap-actual-heap (gethash heap-id *heaps*))))
              (equal (aref (heap-actual-heap (gethash heap-id *heaps*))
                           (heap-size (gethash heap-id *heaps*)))
                     nil))
         (setf (aref (heap-actual-heap (gethash heap-id *heaps*))
                     (heap-size (gethash heap-id *heaps*)))
               (list inf v))
         (setf (gethash (list 'INDEX heap-id (list k v))
                        *indices*)
               (heap-size (gethash heap-id *heaps*)))
         (setf (gethash heap-id *heaps*)
               (list 'HEAP
                     heap-id
                     (+ (heap-size (gethash heap-id *heaps*)) 1)
                     (heap-actual-heap (gethash heap-id *heaps*))))
         (heap-decrease-key heap-id
                            (- (heap-size (gethash heap-id *heaps*)) 1)
                            k))
        (T (error "HEAP FULL ERROR"))))



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
(defun heap-decrease-key (heap-id i k)
  (cond ((>= (first (aref (heap-actual-heap (gethash heap-id *heaps*)) i))
             k)
         (remhash (list 'INDEX
                        heap-id
                        (aref (heap-actual-heap (gethash heap-id *heaps*)) i))
                  *indices*)
         (setf (gethash (list 'INDEX
                              heap-id
                              (list k
                                    (second (aref (heap-actual-heap (gethash
                                                                     heap-id
                                                                     *heaps*))
                                                  i))))
                        *indices*)
               i)
         (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) i)
               (list k (second (aref (heap-actual-heap (gethash heap-id
                                                                *heaps*))
                                     i))))
         (heap-decrease-key-shift-up heap-id i))
        (T (error "THE NEW KEY IS GREATER"))))



(defun heap-decrease-key-shift-up (heap-id i)
  (cond ((and (> i 0)
              (> (first (aref (heap-actual-heap (gethash heap-id
                                                         *heaps*))
                              (floor (- i 1) 2)))
                 (first (aref (heap-actual-heap (gethash heap-id
                                                         *heaps*))
                              i))))
         (heap-switch heap-id (floor (- i 1) 2) i)
         (heap-decrease-key-shift-up heap-id (floor (- i 1) 2)))
        (T T)))



;;; Switch the the entry in position i in the heap identified
;;; by heap-id with the one on position j and vice versa.
(defun heap-switch (heap-id i j)
  (let ((vi (aref (heap-actual-heap (gethash heap-id *heaps*)) i))
        (vj (aref (heap-actual-heap (gethash heap-id *heaps*)) j)))
    (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) i) vj)
    (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) j) vi)
    (setf (gethash (list 'INDEX heap-id vi) *indices*) j)
    (setf (gethash (list 'INDEX heap-id vj) *indices*) i)))



;;; HEAP-EXTRACT-MIN
;;;
;;; heap-extract-min(A)
;;;     if A.heap-size < 1
;;;         error "heap underflow"
;;;     min = A[1]
;;;     A[1] = A[A.heapsize]
;;;     A.heapsize = A.heapsize - 1
;;;     min-heapify(A, 0)
;;;     return min
;;;
(defun heap-extract (heap-id)
  (cond ((heap-empty heap-id)
         (error "HEAP UNDERFLOW ERROR"))
        (T (setf (gethash heap-id *heaps*)
                 (list 'HEAP
                       heap-id
                       (- (heap-size (gethash heap-id *heaps*)) 1)
                       (heap-actual-heap (gethash heap-id *heaps*))))
           (heap-switch heap-id 0 (heap-size (gethash heap-id *heaps*)))
           (remhash (list 'INDEX
                          heap-id
                          (aref (heap-actual-heap (gethash heap-id
                                                           *heaps*))
                                (heap-size (gethash heap-id
                                                    *heaps*))))
                    *indices*)
           (heapify heap-id 0)
           (car (cons (aref (heap-actual-heap (gethash heap-id *heaps*))
                            (heap-size (gethash heap-id *heaps*)))
                      (setf (aref (heap-actual-heap (gethash heap-id *heaps*))
                                  (heap-size (gethash heap-id *heaps*)))
                            nil))))))



;;; MIN-HEAPIFY
;;;
;;; min-heapify(A, i)
;;;     l = left(i)
;;;     r = right(i)
;;;     if l < A.heapsize AND A[l] < A[i]
;;;         minimum = l
;;;     else minimum = i
;;;     if r < A.heapsize AND A[r] < A[minimum]
;;;         minimum = r
;;;     if minimum /= i
;;;         heap-switch A[i] with A[minimum]
;;;     min-heapify(A, minimum)
;;;
(defun heapify (heap-id i)
  (let ((l (+ (* i 2) 1))
        (r (+ (* i 2) 2))
        (heap-rep (gethash heap-id *heaps*)))
    (cond ((and (< l (heap-size heap-rep))
                (< (first (aref (heap-actual-heap heap-rep) l))
                   (first (aref (heap-actual-heap heap-rep) i))))
           (cond ((and
                   (< r (heap-size heap-rep))
                   (< (first (aref (heap-actual-heap heap-rep) r))
                      (first (aref (heap-actual-heap heap-rep) l))))
                  (heap-switch heap-id i r)
                  (heapify heap-id r))
                 (T (heap-switch heap-id i l)
                    (heapify heap-id l))))
          ((and (< r (heap-size heap-rep))
                (< (first (aref (heap-actual-heap heap-rep) r))
                   (first (aref (heap-actual-heap heap-rep) i))))
           (heap-switch heap-id i r)
           (heapify heap-id r)))))



;;; Write the given heap in the standard output with the following notation:
;;; heap-id \n
;;; heap-size \n
;;; heap-actual-heap \n\n
;;; heap-id's entry in *heaps*
(defun heap-print (heap-id)
  (format t "HEAP-ID:~t~a~%" heap-id)
  (format t "HEAP-SIZE:~t~a~%" (third (gethash heap-id *heaps*)))
  (format t "HEAP-ACTUAL-HEAP:~%~t~a~%~%" (fourth (gethash heap-id *heaps*)))
  (format t
          "*heaps* entry for ~a:~%~a =~{~t~a~}"
          heap-id
          heap-id
          (gethash heap-id *heaps*)))



;;;; MST IMPLEMENTATION


;;; Return the weight of the arc connecting vertex-id to the MST.
(defun mst-vertex-key (graph-id vertex-id)
  (let ((record (gethash (list 'VERTEX-KEY
                               graph-id
                               vertex-id)
                         *vertex-keys*)))
    (cond (record
           (fourth record))
          (T inf))))



;;; Return the vertex-id's "parent" vertex in the MST algorithm.
(defun mst-previous (graph-id vertex-id)
  (let ((record (gethash (list 'PREVIOUS graph-id vertex-id) *previous*)))
    (cond (record
           (third record))
          (T nil))))



;;; Executes the Prim's algorithm by adding in the hash tables *vertex-keys*
;;; and *previous* the respective elements.
(defun mst-prim (graph-id source-id)
  (prim-reset graph-id)
  (cond ((has-vertex graph-id source-id)
         (new-heap graph-id (length (graph-vertices graph-id)))
         (mapcar #'(lambda (v)
                     (cond ((equal (second v) graph-id)
                            (cond ((equal (third v) source-id)
                                   (setf (gethash (list 'VERTEX-KEY
                                                        graph-id
                                                        (third v))
                                                  *vertex-keys*)
                                         (list 'VERTEX-KEY
                                               graph-id
                                               (third v)
                                               0))
                                   (heap-insert graph-id 0 (third v)))
                                  (T (setf (gethash (list 'VERTEX-KEY
                                                          graph-id
                                                          (third v))
                                                    *vertex-keys*)
                                           (list 'VERTEX-KEY
                                                 graph-id
                                                 (third v)
                                                 inf))
                                     (heap-insert graph-id inf (third v)))))
                           (T nil)))
                 (graph-vertices graph-id))
         (mst-prim-recurse graph-id))
        (T (error "GRAPH MISMATCH"))))




;;; Support function for mst-prim.
;;; Executes the recursively the iterating part of the algorithm.
  (defun mst-prim-recurse (graph-id)
    (cond ((heap-not-empty graph-id)
           (let ((minimum (heap-extract graph-id)))
             (setf (gethash (list 'VISITED graph-id (second minimum))
                            *visited*)
                   (list 'VERTEX graph-id (second minimum)))
             (mapcar #'(lambda (arc)
                         (cond ((and (equal (gethash (list 'VISITED
                                                           graph-id
                                                           (fourth arc))
                                                     *visited*)
                                            nil)
                                     (< (fifth arc)
                                        (fourth (gethash (list 'VERTEX-KEY
                                                               graph-id
                                                               (fourth arc))
                                                         *vertex-keys*))))
                                (heap-decrease-key
                                 graph-id
                                 (hashed-heap-first-index
                                  graph-id
                                  (fourth (gethash (list 'VERTEX-KEY
                                                         graph-id
                                                         (fourth arc))
                                                   *vertex-keys*))
                                  (fourth arc)
                                  0)
                                 (fifth arc))
                                (setf (gethash (list 'PREVIOUS
                                                     graph-id
                                                     (fourth arc))
                                               *previous*)
                                      (list 'VERTEX
                                            graph-id
                                            (second minimum)))
                                (setf (gethash (list 'VERTEX-KEY
                                                     graph-id
                                                     (fourth arc))
                                               *vertex-keys*)
                                      (list 'VERTEX-KEY
                                            graph-id
                                            (fourth arc)
                                            (fifth arc))))
                               (T nil)))
                     (graph-vertex-neighbors graph-id (second minimum))))
           (mst-prim-recurse graph-id))
          (T nil)))



;;; Support function for mst-prim-recurse.
;;; Extract the array from heap-id and starting from start-index finds the
;;; lowest index where the element with the given value is present.
(defun heap-first-index (heap-id key value start-index)
  (print "test")
  (cond ((<= (heap-size (gethash heap-id *heaps*))
             start-index)
         nil)
        ((> (first (aref (heap-actual-heap (gethash heap-id *heaps*))
                         start-index))
            key)
         nil)
        ((equal (aref (heap-actual-heap (gethash heap-id *heaps*))
                      start-index)
                (list key value))
         start-index)
        (T (or (heap-first-index heap-id
                                 key
                                 value
                                 (+ (* 2 start-index) 1))
               (heap-first-index heap-id
                                 key
                                 value
                                 (+ (* 2 start-index) 2))))))



;;; Search indices in the *indices* hashtable before searching the element in
;;; the heap
(defun hashed-heap-first-index (heap-id key value start-index)
  (or (gethash (list 'INDEX
                     heap-id
                     (list key value))
               *indices*)
      (setf (gethash (list 'INDEX
                           heap-id
                           (list key value))
                     *indices*)
            (heap-first-index heap-id key value start-index))))



;;; Return the MST graph following a preorder visit, arcs with equal weights
;;; are lexicographically ordered.
(defun mst-get (graph-id source-id)
  (mapcan #'(lambda (arc)
              (append (list arc) (mst-get graph-id (third arc))))
          (get-prim-arcs graph-id source-id)))



;;; Create a list of arcs from the hash tables *vertex-keys* and *previous*
;;; that the function mst-prim has populated.
(defun get-prim-arcs (graph-id source-id)
  (let ((acc ()))
    (maphash #'(lambda (k v)
                 (cond ((equal (third v) source-id)
                        (push (list 'ARC
                                    (third v)
                                    (third k)
                                    (fourth (gethash (list 'VERTEX-KEY
                                                           graph-id
                                                           (third k))
                                                     *vertex-keys*)))
                              acc))
                       (T nil)))
             *previous*)
    (stable-sort (stable-sort acc
                              #'STRING< :KEY
                              #'(lambda (arc)
                                  (write-to-string (third arc))))
                 #'< :KEY #'FOURTH)))



;;; Clear all the hash tables related to the Prim's algorithm
(defun prim-reset (heap-id)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (cond ((equal k heap-id)
                      (remhash k *heaps*))))
           *heaps*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) heap-id)
                      (remhash k *vertex-keys*))))
           *vertex-keys*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) heap-id)
                      (remhash k *previous*))))
           *previous*)
  (maphash #'(lambda (k v)
               (cond ((equal (second v) heap-id)
                      (remhash k *visited*))))
           *visited*))



;;; Return the sum of the weight of all the arcs making the MST solution
(defun mst-weight (graph-id source-id)
  (apply #'+ (mapcar #'(lambda (l)
                         (fourth l))
                     (mst-get graph-id source-id))))



;;; Create a graph reading arcs from a file or adds the arcs to the graph
;;; if already exixsting
(defun read-graph (graph-id file-name)
  (new-graph graph-id)
  (with-open-file (in file-name :direction :input :if-does-not-exist :error)
    (new-arcs-from graph-id in)))



;;; Adds arcs to a pre-existing graph reading from a stream
(defun new-arcs-from (graph-id input-stream)
  (let ((v (read input-stream nil nil))
        (u (read input-stream nil nil))
        (w (read input-stream nil nil)))
    (unless (or (eq v nil)
                (eq u nil)
                (eq w nil))
      (new-vertex graph-id v)
      (new-vertex graph-id u)
      (new-arc graph-id v u w)
      (new-arcs-from graph-id  input-stream))))



;;;; end of file -- mst.lisp --
