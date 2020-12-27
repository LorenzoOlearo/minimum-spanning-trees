;;;; Demo data structures and function for mst.lisp


;;; Create the graph shown in chapter 23.2 at page 523 under the Prim's
;;; algorithm explanation.
(defun load-demo-graph (graph-id)
  (new-graph graph-id)
  (new-vertex graph-id 'a)
  (new-vertex graph-id 'b)
  (new-vertex graph-id 'c)
  (new-vertex graph-id 'd)
  (new-vertex graph-id 'e)
  (new-vertex graph-id 'f)
  (new-vertex graph-id 'g)
  (new-vertex graph-id 'h)
  (new-vertex graph-id 'i)
  (new-arc graph-id 'a 'b 4)
  (new-arc graph-id 'a 'h 8)
  (new-arc graph-id 'h 'b 11)
  (new-arc graph-id 'h 'i 7)
  (new-arc graph-id 'h 'g 1)
  (new-arc graph-id 'b 'c 8)
  (new-arc graph-id 'i 'c 2)
  (new-arc graph-id 'i 'g 6)
  (new-arc graph-id 'g 'f 2)
  (new-arc graph-id 'c 'f 4)
  (new-arc graph-id 'f 'e 10)
  (new-arc graph-id 'f 'd 14)
  (new-arc graph-id 'c 'd 7)
  (new-arc graph-id 'd 'e 9))



;;; Create a new simple heap.
(defun load-demo-heap (heap-id)
  (cond ((gethash heap-id *heaps*)
         (heap-delete heap-id)))
  (new-heap heap-id 20)
  (heap-insert heap-id 1 'one)
  (heap-insert heap-id 8 'eight)
  (heap-insert heap-id 3 'three)
  (heap-insert heap-id 2 'two)
  (heap-insert heap-id 10 'ten)
  (heap-insert heap-id 5 'five))



;;; Demo function for primkiller graphs.
;;; For each arc add the corresponding vertices.
(defun demo-new-arc (graph-id v u &optional (weight 1))
  (new-graph)
  (new-vertex graph-id v)
  (new-vertex graph-id u)
  (remhash (list 'arc graph-id u v) *arcs*)
  (setf (gethash (list 'arc graph-id v u) *arcs*)
        (list 'arc graph-id v u weight)))
