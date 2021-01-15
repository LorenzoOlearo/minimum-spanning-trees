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


;;; Tests if a graph named graph-id exists.
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



;;; Tests if a vertex is in a graph
(defun has-vertex (graph-id vertex-id)
  (equal (second (gethash (list 'VERTEX graph-id vertex-id) *vertices*))
         graph-id))



;;; Return a list containing all the vertices in a given graph.
(defun graph-vertices (graph-id)
  (let ((acc ()))
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



;; PLACEHOLDER while waiting for updated specifics.
;; How at least one of the two neighbors function should look like.
(defun fixed-graph-vertex-adjacent (graph-id vertex-id)
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
                             (list 'ARC
                                   graph-id
                                   vertex-id
                                   (third u)))
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
  (cond ((is-graph graph-id)
         (format t "~a~%~%" graph-id)
         (format t "~{~a~%~}~%" (graph-vertices graph-id))
         (format t "~{~a~%~}" (graph-arcs graph-id)))
        (T nil)))



;;;; MINHEAP IMPLEMENTATION


;;; Create a new heap in the hashtable *heaps*.
;;; ---> heap-rep: (HEAP heap-id heap-size actual-heap)
(defun new-heap (heap-id &optional (capacity default-heap-size))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity
                                              :initial-element nil)))))



;;; Increases the capacity of a heap.
(defun heap-increase-capacity (heap-id &optional (capacity 1))
  (let ((size (heap-size (gethash heap-id *heaps*)))
        (actual-heap (heap-actual-heap (gethash heap-id *heaps*))))
    (heap-delete heap-id)
    (setf (gethash heap-id *heaps*)
          (list 'HEAP heap-id size (make-array (+ (length actual-heap)
                                                  capacity)
                                               :initial-element nil)))
    (copy-array actual-heap (heap-actual-heap (gethash heap-id *heaps*)))))



;;; Copies the content of an array onto another.
(defun copy-array (original copy &optional (start 0))
  (cond ((and (< start (length original))
              (>= (length copy) (length original)))
         (setf (aref copy start) (aref original start))
         (copy-array original copy (+ start 1)))
        (T nil)))



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
(defun heap-head-extended (heap-id)
  (heap-get heap-id 0))



(defun heap-head (heap-id)
  (list (heap-key (heap-head-extended heap-id))
        (heap-value (heap-head-extended heap-id))))



;;; Returns the key of a heap-element (key value) or (key (value ...)).
(defun heap-key (heap-element)
  (first heap-element))



;;; Returns the value of a heap-element (key (value ...))
;;; This function is supposed to never be used with non extended heap
;;; functions (e.g. (heap-value (heap-head heap-id)) is not proper usage,
;;; (heap-value (heap-head-extended heap-id)) is proper usage)
(defun heap-value (heap-element)
  (first (heap-value-extended heap-element)))



;;; Returns the list (value ...) of a heap-element (key (value ...))
;;; This function is supposed to never be used with non extended heap
;;; functions (e.g. (heap-value-extended (heap-head heap-id)) is not proper
;;; usage, (heap-value-extended (heap-head-extended heap-id)) is proper
;;; usage)
;;; Although, it is noted, how (heap-value-extended (heap-head heap-id))
;;; returns the same value as (heap-value (heap-head-extended heap-id))
(defun heap-value-extended (heap-element)
  (second heap-element))



;;; Returns a heap-element (key (value ...)) stored in a heap in the array
;;; position index
(defun heap-get (heap-id index)
  (aref (heap-actual-heap (gethash heap-id *heaps*)) index))



;;; Add an element to the heap while maintaining the min heap property.
;;;
;;; Min-Heap-Insert(A, Key)
;;;     A.heap-size = A.heap-size + 1
;;;     A[A.heap-size] = -inf
;;;     Heap-Decrease-Key(A, (A.heap-size - 1), key)
;;;
(defun heap-insert-extended (heap-id k v)
  (let ((val (if (listp v)
                 v
                 (list v))))
    (cond ((and (< (heap-size (gethash heap-id *heaps*))
                   (length (heap-actual-heap (gethash heap-id *heaps*))))
                (equal (heap-get heap-id
                                 (heap-size (gethash heap-id
                                                     *heaps*)))
                       nil))
           (setf (aref (heap-actual-heap (gethash heap-id *heaps*))
                       (heap-size (gethash heap-id *heaps*)))
                 (list inf val))
           (setf (gethash (list 'INDEX heap-id (list k (first val)))
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
          (T (heap-increase-capacity heap-id)
             (heap-insert-extended heap-id k v)))))



(defun heap-insert (heap-id k v)
  (heap-insert-extended heap-id k (list v)))



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
  (cond ((>= (heap-key (heap-get heap-id i))
             k)
         (remhash (list 'INDEX
                        heap-id
                        (list (heap-key (heap-get heap-id i))
                              (heap-value (heap-get heap-id i))))
                  *indices*)
         (setf (gethash (list 'INDEX
                              heap-id
                              (list k
                                    (heap-value (heap-get heap-id i))))
                        *indices*)
               i)
         (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) i)
               (list k (heap-value-extended (heap-get heap-id i))))
         (heap-decrease-key-shift-up heap-id i))
        (T (error "THE NEW KEY IS GREATER"))))



;;; Support function for heap-decrease-key.
;;; shift a heap entry up in the heap according to its key to keep heap
;;; property
(defun heap-decrease-key-shift-up (heap-id i)
  (cond ((and (> i 0)
              (> (heap-key (heap-get heap-id (floor (- i 1) 2)))
                 (heap-key (heap-get heap-id i))))
         (heap-switch heap-id (floor (- i 1) 2) i)
         (heap-decrease-key-shift-up heap-id (floor (- i 1) 2)))
        (T T)))



;;; Switch the the entry in position i in the heap identified
;;; by heap-id with the one on position j and vice versa.
(defun heap-switch (heap-id i j)
  (let ((vi (heap-get heap-id i))
        (vj (heap-get heap-id j)))
    (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) i) vj)
    (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) j) vi)
    (setf (gethash (list 'INDEX heap-id (list (heap-key vi)
                                              (heap-value vi)))
                   *indices*)
          j)
    (setf (gethash (list 'INDEX heap-id (list (heap-key vj)
                                              (heap-value vj)))
                   *indices*)
          i)))



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
(defun heap-extract-extended (heap-id)
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
                          (list (heap-key (heap-get
                                           heap-id
                                           (heap-size (gethash heap-id
                                                               *heaps*))))
                                (heap-value (heap-get
                                             heap-id
                                             (heap-size (gethash heap-id
                                                                 *heaps*))))))
                    *indices*)
           (heapify heap-id 0)
           (car (cons (heap-get heap-id (heap-size (gethash heap-id *heaps*)))
                      (setf (aref (heap-actual-heap (gethash heap-id *heaps*))
                                  (heap-size (gethash heap-id *heaps*)))
                            nil))))))



(defun heap-extract (heap-id)
  (let ((record (heap-extract-extended heap-id)))
    (list (heap-key record) (heap-value record))))



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
                (< (heap-key (heap-get heap-id l))
                   (heap-key (heap-get heap-id i))))
           (cond ((and
                   (< r (heap-size heap-rep))
                   (< (heap-key (heap-get heap-id r))
                      (heap-key (heap-get heap-id l))))
                  (heap-switch heap-id i r)
                  (heapify heap-id r))
                 (T (heap-switch heap-id i l)
                    (heapify heap-id l))))
          ((and (< r (heap-size heap-rep))
                (< (heap-key (heap-get heap-id r))
                   (heap-key (heap-get heap-id i))))
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
  (let ((record (gethash (list 'VERTEX-KEY graph-id vertex-id)
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
         (mst-prim-init graph-id source-id)
         (mst-prim-recurse graph-id source-id))
        (T (error "GRAPH MISMATCH"))))



;;; Support function for mst-prim.
;;; initializes the heap and the *vertex-keys* hash-table.
(defun mst-prim-init (graph-id source-id)
  (mapcar #'(lambda (v)
              (setf (gethash (list 'VERTEX-KEY
                                   graph-id
                                   (third v))
                             *vertex-keys*)
                    (list 'VERTEX-KEY
                          graph-id
                          (third v)
                          (if (equal (third v) source-id)
                              0
                              inf)))
              (heap-insert-extended
               graph-id
               (if (equal (third v) source-id)
                   0
                   inf)
               (list (third v))))
          (graph-vertices graph-id))
  (mapcar #'(lambda (arc)
              (let ((val-src (heap-value-extended (heap-get
                                                   graph-id
                                                   (hashed-heap-first-index
                                                    graph-id
                                                    (if (equal (third arc)
                                                               source-id)
                                                        0
                                                        inf)
                                                    (third arc)
                                                    0))))
                    (val-dst (heap-value-extended (heap-get
                                                   graph-id
                                                   (hashed-heap-first-index
                                                    graph-id
                                                    (if (equal (fourth arc)
                                                               source-id)
                                                        0
                                                        inf)
                                                    (fourth arc)
                                                    0)))))
                (setf (aref (heap-actual-heap (gethash graph-id *heaps*))
                            (hashed-heap-first-index graph-id
                                                     (if (equal (third arc)
                                                                source-id)
                                                         0
                                                         inf)
                                                     (third arc)
                                                     0))
                      (list (if (equal (third arc)
                                       source-id)
                                0
                                inf)
                            (list (first val-src)
                                  (append (second val-src)
                                          (list (list (fourth arc)
                                                      (fifth arc)))))))
                (setf (aref (heap-actual-heap (gethash graph-id *heaps*))
                            (hashed-heap-first-index graph-id
                                                     (if (equal (fourth arc)
                                                                source-id)
                                                         0
                                                         inf)
                                                     (fourth arc)
                                                     0))
                      (list (if (equal (fourth arc)
                                       source-id)
                                0
                                inf)
                            (list (first val-dst)
                                  (append (second val-dst)
                                          (list (list (third arc)
                                                      (fifth arc)))))))))
          (graph-arcs graph-id)))



;;; Support function for mst-prim.
;;; Executes the recursively the iterating part of the algorithm.
(defun mst-prim-recurse (graph-id source-id)
  (cond ((and (heap-not-empty graph-id)
              (or (gethash (list 'PREVIOUS
                                 graph-id
                                 (heap-value (heap-head-extended graph-id)))
                           *previous*)
                  (equal (heap-value (heap-head-extended graph-id))
                         source-id)))
         (let ((minimum (heap-extract-extended graph-id)))
           (setf (gethash (list 'VISITED
                                graph-id
                                (heap-value minimum))
                          *visited*)
                 (list 'VERTEX graph-id (heap-value minimum)))
           (mapcar #'(lambda (arc)
                       (cond ((and (equal (gethash (list 'VISITED
                                                         graph-id
                                                         (first arc))
                                                   *visited*)
                                          nil)
                                   (< (second arc)
                                      (fourth (gethash (list 'VERTEX-KEY
                                                             graph-id
                                                             (first arc))
                                                       *vertex-keys*))))
                              (heap-decrease-key
                               graph-id
                               (hashed-heap-first-index
                                graph-id
                                (fourth (gethash (list 'VERTEX-KEY
                                                       graph-id
                                                       (first arc))
                                                 *vertex-keys*))
                                (first arc)
                                0)
                               (second arc))
                              (setf (gethash (list 'PREVIOUS
                                                   graph-id
                                                   (first arc))
                                             *previous*)
                                    (list 'VERTEX
                                          graph-id
                                          (heap-value minimum)))
                              (setf (gethash (list 'VERTEX-KEY
                                                   graph-id
                                                   (first arc))
                                             *vertex-keys*)
                                    (list 'VERTEX-KEY
                                          graph-id
                                          (first arc)
                                          (second arc))))
                             (T nil)))
                   (second (heap-value-extended minimum))))
         (mst-prim-recurse graph-id source-id))
        ((heap-delete graph-id)
         nil)))



;;; Support function for mst-prim-recurse.
;;; Extract the array from heap-id and starting from start-index finds the
;;; lowest index where the element with the given value is present.
(defun heap-first-index (heap-id key value start-index)
  (cond ((<= (heap-size (gethash heap-id *heaps*))
             start-index)
         nil)
        ((> (heap-key (heap-get heap-id start-index))
            key)
         nil)
        ((and (equal (heap-key (heap-get heap-id start-index))
                     key)
              (equal (heap-value (heap-get heap-id start-index))
                     value))
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
                 (cond ((and (equal (second v) graph-id)
                             (equal (third v) source-id))
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
                              #'LEXICOGRAPHIC< :KEY
                              #'THIRD)
                 #'< :KEY #'FOURTH)))



(defun lexicographic< (a b)
  (cond ((and (realp a) (realp b))
         (< a b))
        (T (string< (if (realp a)
                        (write-to-string a)
                        a)
                    (if (realp b)
                        (write-to-string b)
                        b)))))



;;; Clear all the hash tables related to the Prim's algorithm
(defun prim-reset (heap-id)
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



;;; Creates a graph reading arcs from a file or adds the arcs to the graph
;;; if already exixsting
(defun read-graph-from-csv (graph-id file-name)
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
