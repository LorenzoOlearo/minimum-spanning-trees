(defparameter *heaps* (make-hash-table :test #'equal))

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-rep)
  (cond ((equal (first heap-rep) 'heap)
         (second heap-rep))
        (T nil)))

(defun heap-size (heap-rep)
  (cond ((equal (first heap-rep) 'heap)
         (third heap-rep))
        (T nil)))

(defun heap-actual-heap (heap-rep)
  (cond ((equal (first heap-rep) 'heap)
         (fourth heap-rep))
        (T nil)))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  (equals 0 (heap-size (gethash heap-id *heaps*))))

(defun heap-not-empty (heap-id)
  (cond ((equals 0 (heap-size (gethash heap-id *heaps*))) nil)
        (T T)))

(defun heap-head (heap-id)
  (first (heap-actual-heap (get-hash heap-id *heaps*))))

(defun heap-insert (id k v)
  (cond ((equal (aref (heap-actual-heap (gethash id *heaps*))
                      (heap-size (gethash id *heaps*)))
                nil)
          (setf (aref (heap-actual-heap (gethash id *heaps*))
                      (heap-size (gethash id *heaps*)))
                (list 'inf v))
          (setf (gethash id *heaps*)
                (list 'heap
                      id
                      (+ 1 (heap-size (gethash id *heaps*)))
                      (heap-actual-heap (gethash id *heaps*))))
          (heap-decrease-key (heap-actual-heap (gethash id *heaps*))
                             (- 1 (heap-size (gethash id *heaps*)))
                             k))
         (T (error "heap is full"))))

(defun heap-decrease-key (heap i k)
  (cond ((or (equal (first (aref heap i)) 'inf)
             (equal (> (aref heap i)) k))
         (setf (aref heap i) (list k (second (aref heap i))))
         (heap-decrease-key-shift-up heap i))
        (T (error "new key is greater"))))

(defun heap-decrease-key-shift-up (heap i)
  (cond ((and (> i 0)
              (> (aref heap (floor i 2)) (aref heap i)))
         (let ((val (aref heap i)) (parent (aref heap (floor i 2))))
           (setf (aref heap (floor i 2)) val)
           (setf (aref heap i) parent))
         (heap-decrease-key-shift-up (heap (floor i 2))))
        (T T)))
